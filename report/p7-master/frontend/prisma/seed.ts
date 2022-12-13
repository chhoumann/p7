import { PrismaClient } from "@prisma/client";
const prisma = new PrismaClient();

async function main() {
    console.log(`Start seeding ...`);

    try {
        await prisma.role.createMany({
            data: [{ name: "student" }, { name: "teacher" }],
        });
    } catch (error) {}

    const syllabusExists = await prisma.syllabus.findFirst({
        where: {
            name: "Programming Paradigms F22",
        },
    });

    if (syllabusExists) {
        console.log(`Syllabus already exists, skipping seeding ...`);
    } else {
        console.log(`Seeding syllabus ...`);
        await addTestSyllabus();
    }

    console.log(`Seeding finished.`);
}

async function addTestSyllabus() {
    await prisma.syllabus.create({
        data: {
            name: "Programming Paradigms F22",
            ProblemSets: {
                create: {
                    topic: "Functions and lists",
                    date: new Date(Date.now()), // Needs to be proper ISO string
                    Problems: {
                        create: [
                            {
                                name: "onlytwo",
                                description: `Define, using pattern matching and without using the length function, a function onlytwo that tells us if a list has precisely two elements – in which case it must return True – or not, in which case it must return False. What is the type of onlytwo?`,
                                template: `onlytwo :: `,
                                Tests: {
                                    create: {
                                        code: `module Session3Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Session3.onlytwo" $ do
        it "returns False when list has 0 elements" $ do
            onlytwo [] \`shouldBe\` False
        it "returns True when list has 2 elements" $ do
            onlytwo [0, 1] \`shouldBe\` True
        it "returns False when list has 3 or more elements" $ do
            onlytwo [0..3] \`shouldBe\` False
            onlytwo [0..10] \`shouldBe\` False`,
                                    },
                                },
                            },
                        ],
                    },
                },
            },
        },
    });
}

main();
