# beacon_chain
# Copyright (c) 2024 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at https://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at https://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

{.used.}

import
  # Standard library
  std/json,
  # Status libraries
  stew/byteutils,
  chronicles,
  taskpools,
  # Third-party
  yaml,
  # Beacon chain internals
  ../../beacon_chain/beacon_chain_db,
  ../../beacon_chain/consensus_object_pools/block_clearance,
  ../../beacon_chain/spec/forks,
  # Test utilities
  ../testutil,
  ./fixtures_utils, ./os_ops

type
  TestStepKind {.pure.} = enum
    NewBlock

  TestStep = object
    case kind: TestStepKind
    of TestStepKind.NewBlock:
      blck: ForkedSignedBeaconBlock

func loadForked(
    s: JsonNode, fork_digests: ForkDigests): (ConsensusFork, string) =
  let
    fork_digest = ForkDigest distinctBase(ForkDigest)
      .fromHex(s["fork_digest"].getStr())
    consensusFork = fork_digests.consensusForkForDigest(fork_digest)
      .expect("Unknown fork " & $fork_digest)
    filename = s["data"].getStr()
  (consensusFork, filename)

proc loadSteps(path: string, fork_digests: ForkDigests): seq[TestStep] =
  let stepsYAML = os_ops.readFile(path/"steps.yaml")
  let steps = yaml.loadToJson(stepsYAML)

  result = @[]
  for step in steps[0]:
    if step.hasKey"new_block":
      let
        s = step["new_block"]
        (consensusFork, filename) = s.loadForked(fork_digests)
        blck = withConsensusFork(consensusFork):
          ForkedSignedBeaconBlock.init(parseTest(
            path/filename & ".ssz_snappy", SSZ,
            consensusFork.SignedBeaconBlock))
      result.add TestStep(
        kind: TestStepKind.NewBlock,
        blck: blck)
    else:
      raiseAssert "Unknown test step: " & $step

proc runTest(suiteName, path: string, consensusFork: static ConsensusFork) =
  test "Light client - Data collection - " & path.relativePath(SszTestsDir):
    let (cfg, unknowns) = readRuntimeConfig(path/"config.yaml")
    doAssert unknowns.len == 0

    let
      initial_state = loadForkedState(
        path/"initial_state.ssz_snappy", consensusFork)
      db = BeaconChainDB.new("", cfg = cfg, inMemory = true)
    defer: db.close()
    ChainDAGRef.preInit(db, initial_state[])

    let
      validatorMonitor = newClone(ValidatorMonitor.init(false, false))
      dag = ChainDAGRef.init(cfg, db, validatorMonitor, {},
        lcDataConfig = LightClientDataConfig(
          serve: true, importMode: LightClientDataImportMode.Full))
      rng = HmacDrbgContext.new()
      taskpool = TaskPool.new()
    var verifier = BatchVerifier.init(rng, taskpool)

    let steps = loadSteps(path, dag.forkDigests[])
    for step in steps:
      case step.kind
      of TestStepKind.NewBlock:
        let added = withBlck(step.blck):
          const nilCallback = (consensusFork.OnBlockAddedCallback)(nil)
          dag.addHeadBlock(verifier, forkyBlck, nilCallback)
        check: added.isOk()

suite "EF - Light client - Data collection" & preset():
  const presetPath = SszTestsDir/const_preset
  for kind, path in walkDir(presetPath, relative = true, checkDir = true):
    let testsPath =
      presetPath/path/"light_client"/"data_collection"/"pyspec_tests"
    if kind != pcDir or not dirExists(testsPath):
      continue
    let consensusFork = forkForPathComponent(path).valueOr:
      test "Light client - Data collection - " & path.relativePath(SszTestsDir):
        skip()
      continue
    for kind, path in walkDir(testsPath, relative = true, checkDir = true):
      withConsensusFork(consensusFork):
        runTest(suiteName, testsPath/path, consensusFork)
