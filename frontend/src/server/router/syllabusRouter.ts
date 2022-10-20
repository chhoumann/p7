import { createRouter } from "./context";
import { z } from "zod";
import ky from "ky";
import { env } from "../../env/server.mjs";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Syllabus } from "@prisma/client";

export const syllabusRouter = createRouter()
    .query("getById", {
        input: z.object({
            id: z.string()
        }),
        output: z.object({
            syllabus:  
        }),
        async resolve({input}){
            const syllabus = await prisma.syllabus.findFirst({
                where: {id: input.id}
            })
            if (syllabus === null) {
                console.log("shits fucked")
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: "syllabus with id " + input.id + "not found."
                })
            }
            return {syllabus}
        }
    })