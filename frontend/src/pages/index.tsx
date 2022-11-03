import Link from "next/link";
import React from "react";

export default function IndexPage() {
    return (
        <div className="flex flex-col justify-center">
            <div className="mx-auto">
                Do you want to go to the{" "}
                <Link href={"/syllabi"}>
                    <a className="font-bold">Syllabus page</a>
                </Link>
                ?
            </div>
        </div>
    );
}
