import type { NextApiRequest, NextApiResponse } from "next";
import ky from "ky";
import z from "zod";
import { env } from "../../env/server.mjs";

const route = async (req: NextApiRequest, res: NextApiResponse) => {
  const body = req.body;

  const parsedRequestBody = z
    .object({
      code: z.string(),
    })
    .safeParse(body);

  if (!parsedRequestBody.success) {
    res.status(400).json({ error: "Invalid request body" });
    return;
  }

  try {
    const webserverResponse = await ky
      .post(`${env.WEBSERVER_ADDRESS}/haskell`, {
        json: { code: parsedRequestBody.data.code },
      })
      .json();

    const parsedResponse = z
      .object({
        result: z.string(),
        success: z.boolean(),
      })
      .safeParse(webserverResponse);

    if (!parsedResponse.success) {
      res.status(400).json({ success: false, error: `Invalid input:\n${parsedRequestBody.data}` });
      return;
    }

    res.status(200).json({ ...parsedResponse.data });
  } catch (error) {
    res.status(500).json({ error: "Internal server error" });
  }
};

export default route;
