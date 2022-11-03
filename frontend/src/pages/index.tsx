import Link from "next/link";
import React from "react";
import Layout from "../components/layout";

export default function IndexPage() {
    return (
        <Layout title={"Home"}>
            <div className="mx-auto">
                Do you want to go to the{" "}
                <Link href={"/syllabi"}>
                    <a className="font-bold">Syllabus page</a>
                </Link>
                ?
            </div>
        </Layout>
    );
}
