open Lib.React;

type state = {
  winter: int,
  photography: int,
  travel: int,
};

let%component make = () => {
  let%hook (count, setCount) =
    Brisk_reconciler.Hooks.state({winter: 0, photography: 0, travel: 0});

  <div className="max-w-md rounded overflow-hidden shadow-lg">
    <img className="w-full" src="/static/sunset.jpg" />
    <div className="px-6 py-4">
      <div className="font-bold text-xl mb-2">
        <span text="The Coldest Sunset" />
      </div>
      <span
        className="text-gray-700 text-base"
        text="Lorem ipsum dolor sit amet, consectetur adipisicing elit. Voluptatibus quia, nulla! Maiores et perferendis eaque, exercitationem praesentium nihil."
      />
    </div>
    <div className="px-6 py-4">
      <span
        onClick={_ =>
          setCount(count => {...count, photography: count.photography + 1})
        }
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 cursor-pointer select-none"
        text={"#photography " ++ string_of_int(count.photography)}
      />
      <span
        onClick={_ => setCount(count => {...count, travel: count.travel + 1})}
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 cursor-pointer select-none"
        text={"#travel " ++ string_of_int(count.travel)}
      />
      <span
        onClick={_ => setCount(count => {...count, winter: count.winter + 1})}
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 cursor-pointer select-none"
        text={"#winter " ++ string_of_int(count.winter)}
      />
    </div>
  </div>;
};
