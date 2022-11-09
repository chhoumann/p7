import { signIn, useSession } from "next-auth/react";
import Link from "next/link";
import React from "react";

function Navbar() {
    const { status, data: session } = useSession();

    return (
        <nav>
            <div className="flex items-center p-4 flex-row align-middle justify-between">
                <div className="flex flex-row align-middle gap-4">
                    <Link href="/">
                        <a className="ml-10 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">
                            Home
                        </a>
                    </Link>

                    {session ? (
                        <Link href="/syllabi">
                            <a className="ml-10 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">
                                Courses
                            </a>
                        </Link>
                    ) : null}
                </div>
                <div>
                    {status !== "loading" && session ? (
                        <UserProfile name={session.user?.name ?? ""} />
                    ) : (
                        <LogInBtn />
                    )}
                </div>
            </div>
        </nav>
    );
}

function UserProfile({ name }: { name: string }) {
    return (
        <div className="flex flex-col rounded-full">
            <span className="mr-2 p-1 px-3 rounded-full text-gray-200 bg-sky-700 outline outline-gray-500 outline-1 font-bold text-lg">
                {name.at(0)}
            </span>
        </div>
    );
}

function LogInBtn() {
    return (
        <button
            className="absolute font-bold link link-underline link-underline-black text-xl right-20 hover:text-sky-400 cursor-pointer"
            onClick={() => signIn("google")}
        >
            Sign in
        </button>
    );
}

export default Navbar;
