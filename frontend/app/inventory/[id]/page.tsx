"use client"

import { useEffect, useState } from "react"
import { useRouter } from "next/navigation"
import { ArrowLeft, Edit, Trash } from "lucide-react"

import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Separator } from "@/components/ui/separator"
import { Skeleton } from "@/components/ui/skeleton"
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
import { useToast } from "@/hooks/use-toast"
import { formatCurrency } from "@/lib/utils"

interface InventoryItemDetails {
  id: number
  name: string
  description: string
  quantity: number
  price: number
  category: string
  supplier: string
  createdAt: string
  updatedAt: string
}

export default function InventoryItemPage({ params }: { params: { id: string } }) {
  const router = useRouter()
  const { toast } = useToast()
  const [item, setItem] = useState<InventoryItemDetails | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const getItem = async () => {
      try {
        // Simulate API call
        await new Promise((resolve) => setTimeout(resolve, 500))

        // Mock data
        setItem({
          id: Number(params.id),
          name: "Laptop",
          description: "High-performance laptop for professional use",
          quantity: 25,
          price: 999.99,
          category: "Electronics",
          supplier: "Acme Inc.",
          createdAt: "2023-04-15T10:30:00Z",
          updatedAt: "2023-04-15T10:30:00Z",
        })
      } catch (error) {
        console.error("Failed to fetch item details:", error)
        toast({
          title: "Error",
          description: "Failed to load item details. Please try again.",
          variant: "destructive",
        })
      } finally {
        setLoading(false)
      }
    }

    getItem()
  }, [params.id, toast])

  const handleDelete = async () => {
    try {
      // Simulate API call
      await new Promise((resolve) => setTimeout(resolve, 500))

      toast({
        title: "Success",
        description: "Item deleted successfully",
      })

      router.push("/inventory")
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
    return (
      <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
        <div className="flex items-center gap-2">
          <Skeleton className="h-10 w-10" />
          <Skeleton className="h-10 w-48" />
        </div>
        <Card>
          <CardHeader>
            <Skeleton className="h-6 w-48" />
            <Skeleton className="h-4 w-32" />
          </CardHeader>
          <CardContent className="space-y-6">
            {Array(4)
              .fill(0)
              .map((_, i) => (
                <div key={i} className="space-y-2">
                  <Skeleton className="h-4 w-24" />
                  <Skeleton className="h-6 w-full" />
                </div>
              ))}
          </CardContent>
        </Card>
      </div>
    )
  }

  if (!item) {
    return (
      <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
        <div className="flex items-center gap-2">
          <Button variant="outline" size="icon" onClick={() => router.back()}>
            <ArrowLeft className="h-4 w-4" />
            <span className="sr-only">Back</span>
          </Button>
          <h2 className="text-3xl font-bold tracking-tight">Item Not Found</h2>
        </div>
        <Card>
          <CardContent className="flex flex-col items-center justify-center py-12">
            <p className="text-muted-foreground">The requested inventory item could not be found.</p>
            <Button className="mt-4" onClick={() => router.push("/inventory")}>
              Return to Inventory
            </Button>
          </CardContent>
        </Card>
      </div>
    )
  }

  return (
    <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
      <div className="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between">
        <div className="flex items-center gap-2">
          <Button variant="outline" size="icon" onClick={() => router.back()}>
            <ArrowLeft className="h-4 w-4" />
            <span className="sr-only">Back</span>
          </Button>
          <h2 className="text-3xl font-bold tracking-tight">{item.name}</h2>
        </div>
        <div className="flex items-center gap-2">
          <Button variant="outline" onClick={() => router.push(`/inventory/${item.id}/edit`)}>
            <Edit className="mr-2 h-4 w-4" />
            Edit
          </Button>
          <AlertDialog>
            <AlertDialogTrigger asChild>
              <Button variant="destructive">
                <Trash className="mr-2 h-4 w-4" />
                Delete
              </Button>
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
                  onClick={handleDelete}
                  className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
                >
                  Delete
                </AlertDialogAction>
              </AlertDialogFooter>
            </AlertDialogContent>
          </AlertDialog>
        </div>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Item Details</CardTitle>
          <CardDescription>Detailed information about this inventory item</CardDescription>
        </CardHeader>
        <CardContent className="space-y-6">
          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h3 className="text-sm font-medium text-muted-foreground">Name</h3>
              <p className="text-base font-medium">{item.name}</p>
            </div>
            <div>
              <h3 className="text-sm font-medium text-muted-foreground">Category</h3>
              <p className="text-base">{item.category}</p>
            </div>
          </div>

          <Separator />

          <div>
            <h3 className="text-sm font-medium text-muted-foreground">Description</h3>
            <p className="text-base">{item.description || "No description provided"}</p>
          </div>

          <Separator />

          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h3 className="text-sm font-medium text-muted-foreground">Supplier</h3>
              <p className="text-base">{item.supplier}</p>
            </div>
            <div>
              <h3 className="text-sm font-medium text-muted-foreground">Quantity</h3>
              <p className="text-base">{item.quantity}</p>
            </div>
          </div>

          <Separator />

          <div>
            <h3 className="text-sm font-medium text-muted-foreground">Price</h3>
            <p className="text-base font-medium">{formatCurrency(item.price)}</p>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
