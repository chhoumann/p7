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
        .get(`${env.WEBSERVER_ADDRESS}/haskell/getResult/${id}`)
        .json();

    const parsedResultResponse =
        jobResultResponseSchema.safeParse(jobResultResponse);

    if (!parsedResultResponse.success) {
        throw new Error(parsedResultResponse.error.message);
    }

    return parsedResultResponse;
}

async function tryGetJobResult(id: string, maxRetries: number, delay: number) {
    for (let i = 0; i < maxRetries; i++) {
        const { data } = await getJobResult(id);

        switch (data.status) {
            case "in progress":
                await sleep(delay);
                break;
            case "not found":
                return {
                    success: false,
                    result: "Job not found",
                };
            case "complete":
                return {
                    success: data.success,
                    result: data.output,
                };
            default:
                break;
        }
    }

    throw new Error("Max retries exceeded");
}

const moduleWrapper = (code: string) => `module Code where\n${code}`;

export const codeRouter = createRouter()
    .mutation("haskell", {
        input: z.object({
            code: z.string(),
            test: z.string(),
            problemId: z.string(),
        }),
        output: z.object({
            success: z.boolean(),
            result: z.string(),
        }),
        async resolve({ input, ctx }) {
            const { session, prisma } = ctx;

            if (!session || !session.user?.id) {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            try {
                const jobSubmitResponse = await ky
                    .post(`${env.WEBSERVER_ADDRESS}/haskell/submit`, {
                        json: {
                            code: moduleWrapper(input.code),
                            test: input.test,
                        },
                    })
                    .json();

                const parsedSubmitResponse =
                    jobSubmitResponseSchema.safeParse(jobSubmitResponse);

                if (!parsedSubmitResponse.success) {
                    return {
                        success: false,
                        result: parsedSubmitResponse.error.message,
                    };
                }

                try {
                    const MAX_ATTEMPTS = 200;
                    const ATTEMPT_DELAY = 500;
                    const jobResultResponse = await tryGetJobResult(
                        parsedSubmitResponse.data.id,
                        MAX_ATTEMPTS,
                        ATTEMPT_DELAY
                    );

                    await prisma.submission.create({
                        data: {
                            code: input.code,
                            output: jobResultResponse.result,
                            success: jobResultResponse.success,
                            problemId: input.problemId,
                            userId: session.user.id,
                        },
                    });

                    return jobResultResponse;
                } catch (error) {
                    throw new trpc.TRPCError({
                        message: "Failed to get job results.",
                        code: "INTERNAL_SERVER_ERROR",
                        cause: error,
                    });
                }
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
            return test("new", input);
        },
    });

async function test(
    type: "old" | "new",
    input: { count: number; code: string; test: string }
): Promise<Awaited<ReturnType<typeof testOld>>> {
    return type === "new"
        ? testNew(input.code, input.test, input.count)
        : testOld(input.code, input.test, input.count);
}

async function testNew(
    code: string,
    test: string,
    count: number
): Promise<Awaited<ReturnType<typeof testOld>>> {
    const timeStart = Date.now();
    let i = 0;

    const ps = Array(count)
        .fill(undefined)
        .map(async () => {
            try {
                console.log(i++)
                const req = await fetch(`${env.WEBSERVER_ADDRESS}/haskell/submit`, {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        code,
                        test,
                    }),
                    cache: "no-store",
                });

                const token = (await req.json() as {id: string}).id;
                const MAX_ATTEMPTS = 200;
                const ATTEMPT_DELAY = 50;

                return tryGetJobResult(token, MAX_ATTEMPTS, ATTEMPT_DELAY);
            } catch {
                return undefined;
            }
        });

    const responses = await Promise.all(ps);
    const fulfilledCount = responses.filter((x) => x !== undefined).length;
    const duration = Date.now() - timeStart;

    return {
        requestCount: count,
        fulfilled: fulfilledCount,
        responses,
        duration,
    };
}

async function testOld(code: string, test: string, count: number) {
    const timeStart = Date.now();

    const ps = Array(count)
        .fill(undefined)
        .map(() => {
            try {
                return fetch(`${env.WEBSERVER_ADDRESS}/haskell/submit`, {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        code,
                        test,
                    }),
                    cache: "no-store",
                }).then(async (res) => res.json());
            } catch {
                return undefined;
            }
        });

    const responses = await Promise.all(ps);
    const fulfilledCount = responses.filter((x) => x !== undefined).length;
    const duration = Date.now() - timeStart;

    return {
        requestCount: count,
        fulfilled: fulfilledCount,
        responses,
        duration,
    };
}
