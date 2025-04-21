import Link from "next/link"
import { Plus } from "lucide-react"

import { Button } from "@/components/ui/button"
import { DataTable } from "@/components/data-table"
import { categoryColumns } from "@/components/category-columns"
import { getCategoryData } from "@/lib/data"

export default async function CategoriesPage() {
  const data = await getCategoryData()

  return (
    <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
      <div className="flex flex-col items-start justify-between space-y-2 md:flex-row md:items-center md:space-y-0">
        <h2 className="text-3xl font-bold tracking-tight">Categories</h2>
        <Link href="/categories/add">
          <Button>
            <Plus className="mr-2 h-4 w-4" />
            Add Category
          </Button>
        </Link>
      </div>
      <div className="space-y-4">
        <DataTable columns={categoryColumns} data={data} searchKey="name" />
      </div>
    </div>
  )
}
