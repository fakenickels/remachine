let projectDir = Sys.getcwd();

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir:
        Filename.concat(
          projectDir,
          Filename.concat("test", "__snapshots__"),
        ),
      projectDir,
    });
});
