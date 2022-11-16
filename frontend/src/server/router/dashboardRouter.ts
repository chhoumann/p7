import { createRouter } from "./context";

export const dashboardRouter = createRouter()
    .middleware(async ({ ctx, next }) => {
        if (ctx.session?.user?.role !== "teacher") {
            throw new Error("Not authorized");
        }

        return next();
    })
    .query("dashboardData", {
        async resolve({ ctx }) {
            const res = await ctx.prisma.problemSet.findMany({
                select: {
                    topic: true,
                    Problems: {
                        select: {
                            name: true,
                            _count: true,
                            Submission: {
                                select: {
                                    user: {
                                        select: {
                                            _count: true,
                                            name: true,
                                        },
                                    },
                                    success: true,
                                },
                            },
                        },
                    },
                },
            });

            const r = res.map((problemSet) => {
                problemSet.Problems = problemSet.Problems.map((problem) => {
                    const seen: {
                        [name: string]: boolean;
                    } = {};

                    problem.Submission = problem.Submission.filter(
                        (submission) => {
                            if (!submission.user || !submission.user.name)
                                return false;

                            const hasSeenUser = seen.hasOwnProperty(
                                submission.user.name
                            );

                            if (hasSeenUser) return false;

                            if (submission.success) {
                                seen[submission.user.name] = true;
                                return true;
                            }

                            const userHasLaterSuccessfulSubmission =
                                problem.Submission.find(
                                    (sub) =>
                                        sub.user.name ===
                                            submission.user.name && sub.success
                                );

                            if (!userHasLaterSuccessfulSubmission) {
                                seen[submission.user.name] = true;
                                return true;
                            }

                            return false;
                        }
                    );

                    return problem;
                });

                return problemSet;
            });

            return r;
        },
    });
