import type { NextApiRequest, NextApiResponse } from "next";
import ky from "ky";

const examples = async (req: NextApiRequest, res: NextApiResponse) => {
  const js = JSON.stringify(req.body);

  console.log(js)

  const f = await ky
    .post("https://p7-workers.up.railway.app/haskell", {
      json: {"code": "testing"}
    })

  res.status(200).json({stdout: await f.text()});
};

export default examples;
