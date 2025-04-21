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
import { Badge } from "@/components/ui/badge"
import { DataTablePagination } from "@/components/data-table-pagination"
import { fetchInventory, deleteInventoryItem } from "@/lib/api"
import { formatCurrency } from "@/lib/utils"
import { useToast } from "@/hooks/use-toast"

interface InventoryItem {
  id: number
  name: string
  description: string
  quantity: number
  price: number
  category: string
  supplier: string
  threshold: number
}

export function InventoryTable() {
  const { toast } = useToast()
  const [items, setItems] = useState<InventoryItem[]>([])
  const [loading, setLoading] = useState(true)
  const [page, setPage] = useState(1)
  const [pageSize, setPageSize] = useState(10)
  const [totalItems, setTotalItems] = useState(0)

  useEffect(() => {
    const getInventory = async () => {
      try {
        // Fetch inventory items from the API with pagination
        const data = await fetchInventory(page, pageSize)
        setItems(data.items)
        setTotalItems(data.total)
      } catch (error) {
        console.error("Failed to fetch inventory:", error)
        toast({
          title: "Error",
          description: "Failed to load inventory items. Please try again.",
          variant: "destructive",
        })
      } finally {
        setLoading(false)
      }
    }

    getInventory()
  }, [page, pageSize, toast])

  const handleDelete = async (id: number) => {
    try {
      // Delete inventory item via the API
      await deleteInventoryItem(id)

      // Update the local state
      setItems(items.filter((item) => item.id !== id))

      toast({
        title: "Success",
        description: "Item deleted successfully",
      })
    } catch (error) {
      console.error("Failed to delete item:", error)
      toast({
        title: "Error",
        description: "Failed to delete item. Please try again.",
        variant: "destructive",
      })
    }
  }

  if (loading) {
    return <div>Loading inventory...</div>
  }

  return (
    <div className="space-y-4">
      <div className="rounded-md border">
        <div className="grid grid-cols-7 gap-4 p-4 text-sm font-medium text-muted-foreground">
          <div>Name</div>
          <div>Category</div>
          <div>Supplier</div>
          <div>Quantity</div>
          <div>Price</div>
          <div>Status</div>
          <div className="text-right">Actions</div>
        </div>
        <div className="divide-y">
          {items.length === 0 ? (
            <div className="p-4 text-center text-sm text-muted-foreground">
              No inventory items found. Add some items to get started.
            </div>
          ) : (
            items.map((item) => (
              <div key={item.id} className="grid grid-cols-7 gap-4 p-4 text-sm">
                <div className="font-medium">
                  <Link href={`/inventory/${item.id}`} className="hover:underline">
                    {item.name}
                  </Link>
                </div>
                <div>{item.category}</div>
                <div>{item.supplier}</div>
                <div>{item.quantity}</div>
                <div>{formatCurrency(item.price)}</div>
                <div>
                  {item.quantity <= item.threshold ? (
                    <Badge variant="destructive">Low Stock</Badge>
                  ) : (
                    <Badge variant="outline">In Stock</Badge>
                  )}
                </div>
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
                        <Link href={`/inventory/${item.id}`}>View details</Link>
                      </DropdownMenuItem>
                      <DropdownMenuItem asChild>
                        <Link href={`/inventory/${item.id}/edit`}>
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
                              This will permanently delete the item "{item.name}". This action cannot be undone.
                            </AlertDialogDescription>
                          </AlertDialogHeader>
                          <AlertDialogFooter>
                            <AlertDialogCancel>Cancel</AlertDialogCancel>
                            <AlertDialogAction
                              onClick={() => handleDelete(item.id)}
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
