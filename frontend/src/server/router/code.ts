import { createRouter } from "./context";
import { z } from "zod";
import ky from "ky";
import { env } from "../../env/server.mjs";
import * as trpc from "@trpc/server";

export const codeRouter = createRouter()
  .mutation("haskell", {
    input: z.object({
      code: z.string(),
      test: z.string()
    }),
    output: z.object({
      success: z.boolean(),
      result: z.string(),
    }),
    async resolve({ input }) {
      try {
        const webserverResponse = await ky
          .post(`${env.WEBSERVER_ADDRESS}/haskell`, {
            json: { code: input.code, test: input.test},
          })
          .json();

        const parsedResponse = z
          .object({
            result: z.string(),
            success: z.boolean(),
          })
          .safeParse(webserverResponse);

        if (!parsedResponse.success) {
          return { success: false, result: parsedResponse.error.message };
        }

        return parsedResponse.data;
      } catch (error) {
        throw new trpc.TRPCError({
          code: "INTERNAL_SERVER_ERROR",
          message: "Could not make request to code runner.",
          cause: error,
        });
      }
    },
  })
  .query("spam", {
    input: z.object({
      count: z.number(),
      code: z.string(),
      test: z.string(),
    }),
    async resolve({ input }) {
      const timeStart = Date.now();

      const ps = Array(input.count)
        .fill(undefined)
        .map(() => {
          try {
            return fetch(`${env.WEBSERVER_ADDRESS}/haskell`, {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({ code: input.code, test: input.test }),
            }).then((res) => res.json());
          } catch {
            return undefined;
          }
        });

      const responses = await Promise.all(ps);
      const fulfilledCount = responses.filter((x) => x !== undefined).length;
      const duration = Date.now() - timeStart;

      return {
        requestCount: input.count,
        fulfilled: fulfilledCount,
        responses,
        duration,
      };
    },
  });
