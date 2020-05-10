Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

let handler = (request: Morph.Request.t) => {
  /* TODO: move to Morph as Morph.Uri.split ??? */
  let path_parts =
    request.target
    |> Uri.of_string
    |> Uri.path
    |> String.split_on_char('/')
    |> List.filter(s => s != "");

  Logs.info(m => m("request.target: %s", request.target));

  switch (request.meth, path_parts) {
  | (`GET, []) =>
  let html = 
  List.concat([
    [
      {|<!DOCTYPE html>|},
      {|<html lang="en">|},
      {|<head>|},
      {|<meta charset="UTF-8">|},
      {|<meta name="viewport" content="width=device-width, initial-scale=1.0">|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/base.min.css"/>|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/utilities.min.css"/>|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/components.min.css"/>|},
      {|<title>Remachine Viz</title></head><body id="root">|},
    ],
    [
      {|<script src="//d3js.org/d3.v5.min.js"></script>|},
      {|<script src="https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js"></script>|},
      {|<script src="https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js"></script>|},
      {|<script type="application/javascript" src="https://unpkg.com/d3-graphviz@3.0.6/build/d3-graphviz.min.js"></script>|},
      {|<script type="application/javascript" src="static/_esy/default/build/default/example/client/Client.bc.js"></script>|},
      {|</body></html>|},
    ],
  ])
  |> List.fold_left((++), "");
    Morph.Response.html(html, Morph.Response.empty)

  | (`GET, ["static", ...path]) =>
    let filepath =
      path |> List.fold_left((acc, part) => acc ++ "/" ++ part, Sys.getcwd());

    Morph.Response.static(filepath, Morph.Response.empty);

  | (_, _) => Morph.Response.not_found(Morph.Response.empty)
  };
};

let http_server = Morph_server_http.make();

let () =
  handler
  |> Morph.start(~servers=[http_server], ~middlewares=[])
  |> Lwt_main.run;
