import { createRouter } from "./context";
import { string, z } from "zod";
import ky from "ky";
import { env } from "../../env/server.mjs";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { ProblemSet } from "@prisma/client";

export const problemSetsRouter = createRouter()
    .query("getBySyllabusId", {
        input:  z.object({
            id: z.string()
        }),
        output: z.object({
            problemSets: z.array(
            z.object({
                id: z.string(),
                topic: z.string(),
                date: z.date(),
                syllabusId: z.string(), 
            })
 )
       }),
        async resolve({input}){
            const problemSets = await prisma.problemSet.findMany({
                where: {syllabusId: input.id}
            })
            if (!problemSets || problemSets.length === 0) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: "no problem sets with id " + input.id + " was found"
                })
            }
            return {problemSets};
        }
    })