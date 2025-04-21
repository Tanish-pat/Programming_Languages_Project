"use client"

import { useEffect, useState } from "react"
import Link from "next/link"
import { Edit, MoreHorizontal, Trash } from "lucide-react"

import { Button } from "@/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "@/components/ui/alert-dialog"
import { DataTablePagination } from "@/components/data-table-pagination"
import { fetchCategories, deleteCategory } from "@/lib/api"
import { useToast } from "@/hooks/use-toast"

interface Category {
  id: number
  name: string
  description: string
  itemCount: number
  createdAt: string
}

export function CategoriesTable() {
  const { toast } = useToast()
  const [categories, setCategories] = useState<Category[]>([])
  const [loading, setLoading] = useState(true)
  const [page, setPage] = useState(1)
  const [pageSize, setPageSize] = useState(10)
  const [totalItems, setTotalItems] = useState(0)

  useEffect(() => {
    const getCategories = async () => {
      try {
        // Fetch categories from the API with pagination
        const data = await fetchCategories(page, pageSize)
        setCategories(data.categories)
        setTotalItems(data.total)
      } catch (error) {
        console.error("Failed to fetch categories:", error)
        toast({
          title: "Error",
          description: "Failed to load categories. Please try again.",
          variant: "destructive",
        })
      } finally {
        setLoading(false)
      }
    }

    getCategories()
  }, [page, pageSize, toast])

  const handleDelete = async (id: number) => {
    try {
      // Delete category via the API
      await deleteCategory(id)

      // Update the local state
      setCategories(categories.filter((category) => category.id !== id))

      toast({
        title: "Success",
        description: "Category deleted successfully",
      })
    } catch (error) {
      console.error("Failed to delete category:", error)
      toast({
        title: "Error",
        description: "Failed to delete category. Please try again.",
        variant: "destructive",
      })
    }
  }

  if (loading) {
    return <div>Loading categories...</div>
  }

  return (
    <div className="space-y-4">
      <div className="rounded-md border">
        <div className="grid grid-cols-4 gap-4 p-4 text-sm font-medium text-muted-foreground">
          <div>Name</div>
          <div>Description</div>
          <div>Items</div>
          <div className="text-right">Actions</div>
        </div>
        <div className="divide-y">
          {categories.length === 0 ? (
            <div className="p-4 text-center text-sm text-muted-foreground">
              No categories found. Add some categories to get started.
            </div>
          ) : (
            categories.map((category) => (
              <div key={category.id} className="grid grid-cols-4 gap-4 p-4 text-sm">
                <div className="font-medium">
                  <Link href={`/categories/${category.id}`} className="hover:underline">
                    {category.name}
                  </Link>
                </div>
                <div className="truncate">{category.description || "No description"}</div>
                <div>{category.itemCount} items</div>
                <div className="flex justify-end">
                  <DropdownMenu>
                    <DropdownMenuTrigger asChild>
                      <Button variant="ghost" size="icon">
                        <MoreHorizontal className="h-4 w-4" />
                        <span className="sr-only">Actions</span>
                      </Button>
                    </DropdownMenuTrigger>
                    <DropdownMenuContent align="end">
                      <DropdownMenuLabel>Actions</DropdownMenuLabel>
                      <DropdownMenuSeparator />
                      <DropdownMenuItem asChild>
                        <Link href={`/categories/${category.id}`}>View details</Link>
                      </DropdownMenuItem>
                      <DropdownMenuItem asChild>
                        <Link href={`/categories/${category.id}/edit`}>
                          <Edit className="mr-2 h-4 w-4" /> Edit
                        </Link>
                      </DropdownMenuItem>
                      <AlertDialog>
                        <AlertDialogTrigger asChild>
                          <DropdownMenuItem onSelect={(e) => e.preventDefault()} className="text-destructive">
                            <Trash className="mr-2 h-4 w-4" /> Delete
                          </DropdownMenuItem>
                        </AlertDialogTrigger>
                        <AlertDialogContent>
                          <AlertDialogHeader>
                            <AlertDialogTitle>Are you sure?</AlertDialogTitle>
                            <AlertDialogDescription>
                              This will permanently delete the category "{category.name}". This action cannot be undone.
                              {category.itemCount > 0 && (
                                <p className="mt-2 font-semibold text-destructive">
                                  Warning: This category contains {category.itemCount} items that will be affected.
                                </p>
                              )}
                            </AlertDialogDescription>
                          </AlertDialogHeader>
                          <AlertDialogFooter>
                            <AlertDialogCancel>Cancel</AlertDialogCancel>
                            <AlertDialogAction
                              onClick={() => handleDelete(category.id)}
                              className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
                            >
                              Delete
                            </AlertDialogAction>
                          </AlertDialogFooter>
                        </AlertDialogContent>
                      </AlertDialog>
                    </DropdownMenuContent>
                  </DropdownMenu>
                </div>
              </div>
            ))
          )}
        </div>
      </div>

      <DataTablePagination
        pageCount={Math.ceil(totalItems / pageSize)}
        page={page}
        onPageChange={setPage}
        pageSize={pageSize}
        onPageSizeChange={setPageSize}
      />
    </div>
  )
}
