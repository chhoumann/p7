import { NextPage } from 'next';
import { useRouter } from 'next/router'
import { trpc } from "../../utils/trpc"
import React, { useState } from 'react'
import Link from 'next/link';


const ProblemId: NextPage = () => {

    const [selectedId, setSelectedId] = useState<string>()

    const router = useRouter();
    const { id } = router.query;
    //const sessions = trpc.useQuery
    const testData = [
        { name: "Broblem1", id: "i21en" },
        { name: "Broblem2", id: "i23en" },
        { name: "Broblem3", id: "i22en" },
        { name: "Broblem4", id: "i24en" },
    ]

    return (
        <div className='container flex justify-center items-center w-full h-[75vh]'>
            <h2 className="absolute top-10">Problems</h2>
            <div className='flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500 overflow-auto'>

                {testData.map((session) =>
                    <React.Fragment key={session.id}>
                        <ExerciseRow {...session} onClick={() => setSelectedId(session.id)} isSelected={session.id === selectedId}/>
                    </React.Fragment>
                )}
                <div className='mb-auto' />
                <div className='flex flex-row gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white'>
                    <Link href={`/problem/create`}>
                        <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                            Create new problem
                        </button>
                    </Link>
                    <Link href={selectedId ? `/problem/${selectedId}`: `/problem/${id}`}>
                        <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                            view
                        </button>
                    </Link>
                    <Link href={selectedId ? `/problem/edit/${selectedId}`: `/problem/${id}`}>
                        <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                            Edit
                        </button>
                    </Link>
                </div>
            </div>
        </div>
    )
}

export default ProblemId;


function ExerciseRow({ id, name, isSelected, onClick }:{ id: string, name: string, isSelected: boolean, onClick: () => void }) {
    return (
        <div className={`p-3 m-3 ${isSelected?'bg-gray-400':"bg-gray-300 hover:bg-gray-400"}`} onClick={onClick}>
            <span>{name}</span>
        </div>
    )
}