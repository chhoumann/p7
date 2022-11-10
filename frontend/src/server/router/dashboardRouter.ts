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
            return ctx.prisma.syllabus.findMany({
                select: {
                    _count: true,
                    name: true,
                    ProblemSets: {
                        select: {
                            _count: true,
                            date: true,
                            id: true,
                            topic: true,
                            Problems: {
                                select: {
                                    _count: true,
                                    id: true,
                                    name: true,
                                    Submission: {
                                        select: {
                                            id: true,
                                            success: true,
                                            problem: {
                                                select: {
                                                    id: true,
                                                    name: true,
                                                }
                                            },
                                            user: {
                                                select: {
                                                    name: true,
                                                }
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            });
        },
    });
