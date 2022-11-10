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
            return await ctx.prisma.problemSet.findMany({
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
                                        }
                                    },
                                    success: true,
                                }
                            }
                        }
                    }
                }
            });
        },
    });
