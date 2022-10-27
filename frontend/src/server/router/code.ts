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
            result: z.string()
        }),
        async resolve({ input }) {
            try {
                const webserverResponse = await ky
                    .post(`${env.WEBSERVER_ADDRESS}/haskell`, {
                        json: { code: input.code, test: input.test },
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
                    cause: error
                })
            }
        }
    })