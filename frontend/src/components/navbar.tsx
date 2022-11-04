import { signIn, useSession } from "next-auth/react";
import React from "react";

function Navbar() {
    const { data: session } = useSession();

    return (
        <nav>
            <div className="flex items-center p-4 flex-row align-middle justify-between">
                <span className="ml-10 text-stone-300 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">
                    Home
                </span>
                <div>
                    {session ? (
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
            className="absolute text-stone-300 font-bold link link-underline link-underline-black text-xl right-20 hover:text-sky-400 cursor-pointer"
            onClick={() => signIn("google")}
        >
            Sign in
        </button>
    );
}

export default Navbar;
