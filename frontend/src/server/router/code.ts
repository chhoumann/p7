import { createRouter } from "./context";
import { z } from "zod";
import ky from "ky";
import { env } from "../../env/server.mjs";
import * as trpc from "@trpc/server";

function sleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

const jobSubmitResponseSchema = z.object({
    id: z.string(),
});

const jobResultResponseSchema = z
    .object({
        status: z.enum(["in progress", "not found"]),
    })
    .or(
        z.object({
            status: z.string().regex(/complete/),
            success: z.boolean(),
            output: z.string(),
        })
    );

                async function getJobResult(id: string) {
                    const jobResultResponse = await ky
                        .get(
                            `${env.WEBSERVER_ADDRESS}/haskell/getResult/${id}`
                        )
                        .json();

                    console.log(jobResultResponse);

                    const parsedResultResponse =
                        jobResultResponseSchema.safeParse(jobResultResponse);

                    if (!parsedResultResponse.success) {
                        throw new Error(parsedResultResponse.error.message);
                    }

                    return parsedResultResponse;
                }

export const codeRouter = createRouter()
    .mutation("haskell", {
        input: z.object({
            code: z.string(),
            test: z.string(),
        }),
        output: z.object({
            success: z.boolean(),
            result: z.string(),
        }),
        async resolve({ input }) {
            try {
                const jobSubmitResponse = await ky
                    .post(`${env.WEBSERVER_ADDRESS}/haskell/submit`, {
                        json: { code: input.code, test: input.test },
                    })
                    .json();

                const parsedSubmitResponse =
                    jobSubmitResponseSchema.safeParse(jobSubmitResponse);

                console.log("Got submit response");

                if (!parsedSubmitResponse.success) {
                    return {
                        success: false,
                        result: parsedSubmitResponse.error.message,
                    };
                }

                console.log("Submit response is good");

                for (let i = 0; i < 200; i++) {
                    const { data } = await getJobResult(parsedSubmitResponse.data.id);

                    if (data.status === "not found") {
                        return {
                            success: false,
                            result: "Job not found.",
                        };
                    }

                    if (data.status === "in progress") {
                        await sleep(100);
                    }

                    if (data.status === "complete") {
                        return {
                            success: data.success,
                            result: data.output,
                        };
                    }
                }

                throw new trpc.TRPCError({
                    message: "Failed to get job results.",
                    code: "INTERNAL_SERVER_ERROR",
                })
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
                            body: JSON.stringify({
                                code: input.code,
                                test: input.test,
                            }),
                        }).then((res) => res.json());
                    } catch {
                        return undefined;
                    }
                });

            const responses = await Promise.all(ps);
            const fulfilledCount = responses.filter(
                (x) => x !== undefined
            ).length;
            const duration = Date.now() - timeStart;

            return {
                requestCount: input.count,
                fulfilled: fulfilledCount,
                responses,
                duration,
            };
        },
    });
