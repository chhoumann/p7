import { createRouter } from "./context";
import { string, z } from "zod";
import ky from "ky";
import { env } from "../../env/server.mjs";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Syllabus } from "@prisma/client";

export const problemRouter = createRouter()
    .query("getByProblemSetId", {
        input:  z.object({
            id: z.string()
        }),
        output: z.object({
            problems: z.array(
            z.object({
                id: z.string(),
                name: z.string(),
                template: z.string(),
                description: z.string(),
                problemSetId: z.string()
            })
 )
       }),
        async resolve({input}){
            const problems = await prisma.problem.findMany({
                where: {problemSetId: input.id}
            })
            if (!problems || problems.length === 0) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: "no problem sets with id " + input.id + " was found"
                })
            }
            return {problems};
        }
    })