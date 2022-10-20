import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Syllabus } from "@prisma/client";

export const syllabusRouter = createRouter().query("getById", {
  input: z.string(),
  async resolve({ input: id }) {
    const syllabus: Syllabus | null = await prisma.syllabus.findFirst({
      where: { id },
    });

    if (!syllabus) {
      throw new trpc.TRPCError({
        code: "NOT_FOUND",
        message: `Syllabus with id ${id} not found`,
      });
    }

    return { syllabus };
  },
});
