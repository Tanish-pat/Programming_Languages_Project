"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Star, Edit, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { ReviewAPI, CustomerAPI, ProductAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"
import { Label } from "@/components/ui/label"
import { Textarea } from "@/components/ui/textarea"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"
import type { ColumnDef } from "@tanstack/react-table"

interface Review {
  reviewId: number
  customerId: number
  productId: string
  rating: number
  comment: string
  customerName?: string
  productName?: string
}

interface Customer {
  id: number
  name: string
  email: string
  age: number
  isActive: boolean
}
interface Product {
  sku: string
  name: string
  description: string
  price: number
  tags: string
}

export default function ReviewsPage() {
  const { toast } = useToast()
  const [reviews, setReviews] = useState<Review[]>([])
  const [customers, setCustomers] = useState<Customer[]>([])
  const [products, setProducts] = useState<Product[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentReview, setCurrentReview] = useState<Review | null>(null)

  useEffect(() => {
    fetchAll()
  }, [])

  const fetchAll = async () => {
    setIsLoading(true)
    try {
      const [reviewData, customerData, productData] = await Promise.all([
        ReviewAPI.getAll(),
        CustomerAPI.getAll(),
        ProductAPI.getAll(),
      ])

      const mappedCustomers: Customer[] = customerData.map((c: any[]) => ({
        id: Number(c[0]),
        name: c[1],
        email: c[2],
        age: Number(c[3]),
        isActive: c[4] === 1 || c[4] === true,
      }))

      const mappedProducts: Product[] = productData.map((p: any[]) => ({
        sku: p[0],
        name: p[1],
        description: p[2],
        price: Number(p[3]),
        tags: p[4],
      }))

      const mappedReviews: Review[] = reviewData.map((r: any[]) => {
        const customer = mappedCustomers.find((c) => c.id === Number(r[1]))
        const product = mappedProducts.find((p) => p.sku === r[2])

        return {
          reviewId: Number(r[0]),
          customerId: Number(r[1]),
          productId: r[2],
          rating: Number(r[3]),
          comment: r[4],
          customerName: customer?.name ?? "Unknown Customer",
          productName: product?.name ?? "Unknown Product",
        }
      })

      setCustomers(mappedCustomers)
      setProducts(mappedProducts)
      setReviews(mappedReviews)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch reviews or related data",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }


  const handleAddNew = () => {
    setCurrentReview({
      reviewId: 0,
      customerId: 0,
      productId: "",
      rating: 5,
      comment: "",
    })
    setIsDialogOpen(true)
  }

  const handleEdit = (review: Review) => {
    setCurrentReview(review)
    setIsDialogOpen(true)
  }

  const handleDelete = (review: Review) => {
    setCurrentReview(review)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentReview) return

    try {
      await ReviewAPI.delete(currentReview.reviewId)
      toast({
        title: "Success",
        description: "Review deleted successfully",
      })
      fetchAll()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete review",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value, type } = e.target as HTMLInputElement

    if (!currentReview) return

    setCurrentReview({
      ...currentReview,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSelectChange = (name: string, value: string) => {
    if (!currentReview) return

    setCurrentReview({
      ...currentReview,
      [name]: name === "customerId" ? Number(value) : value,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentReview) return

    try {
      if (currentReview.reviewId === 0) {
        await ReviewAPI.create(currentReview)
        toast({
          title: "Success",
          description: "Review created successfully",
        })
      } else {
        await ReviewAPI.update(currentReview.reviewId, currentReview)
        toast({
          title: "Success",
          description: "Review updated successfully",
        })
      }
      setIsDialogOpen(false)
      fetchAll()
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${currentReview.reviewId === 0 ? "create" : "update"} review`,
        variant: "destructive",
      })
    }
  }

  const renderStars = (rating: number) => {
    return Array(5)
      .fill(0)
      .map((_, i) => (
        <Star key={i} className={`h-4 w-4 ${i < rating ? "text-yellow-400 fill-yellow-400" : "text-gray-300"}`} />
      ))
  }

  const columns: ColumnDef<Review>[] = [
    {
      accessorKey: "reviewId",
      header: "ID",
    },
    {
      accessorKey: "customerName",
      header: "Customer",
    },
    {
      accessorKey: "productName",
      header: "Product",
    },
    {
      accessorKey: "rating",
      header: "Rating",
      cell: ({ row }) => <div className="flex">{renderStars(row.original.rating)}</div>,
    },
    {
      accessorKey: "comment",
      header: "Comment",
      cell: ({ row }) => {
        const comment = row.original.comment
        return comment.length > 50 ? `${comment.substring(0, 50)}...` : comment
      },
    },
    {
      id: "actions",
      cell: ({ row }) => (
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="ghost" size="icon">
              <MoreHorizontal className="h-4 w-4" />
              <span className="sr-only">Open menu</span>
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            <DropdownMenuItem onClick={() => handleEdit(row.original)}>
              <Edit className="mr-2 h-4 w-4" /> Edit
            </DropdownMenuItem>
            <DropdownMenuItem onClick={() => handleDelete(row.original)}>
              <Trash className="mr-2 h-4 w-4" /> Delete
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      ),
    },
  ]

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold tracking-tight">Reviews</h1>
        <Button onClick={handleAddNew}>
          <Star className="mr-2 h-4 w-4" /> Add Review
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Review Management</CardTitle>
          <CardDescription>View and manage product reviews</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading reviews...</div>
          ) : (
            <DataTable
              columns={columns}
              data={reviews}
              searchColumn="productName"
              searchPlaceholder="Search by product name..."
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>{currentReview?.reviewId === 0 ? "Add New Review" : "Edit Review"}</DialogTitle>
              <DialogDescription>
                {currentReview?.reviewId === 0 ? "Add a new product review" : "Edit the review information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="customerId">Customer</Label>
                <Select
                  value={currentReview?.customerId.toString()}
                  onValueChange={(value) => handleSelectChange("customerId", value)}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Select a customer" />
                  </SelectTrigger>
                  <SelectContent>
                    {customers.map((customer) => (
                      <SelectItem key={customer.id} value={customer.id.toString()}>
                        {customer.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="productId">Product</Label>
                <Select
                  value={currentReview?.productId}
                  onValueChange={(value) => handleSelectChange("productId", value)}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Select a product" />
                  </SelectTrigger>
                  <SelectContent>
                    {products.map((product) => (
                      <SelectItem key={product.sku} value={product.sku}>
                        {product.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="rating">Rating</Label>
                <Select
                  value={currentReview?.rating.toString()}
                  onValueChange={(value) => handleSelectChange("rating", value)}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Select a rating" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="1">1 Star</SelectItem>
                    <SelectItem value="2">2 Stars</SelectItem>
                    <SelectItem value="3">3 Stars</SelectItem>
                    <SelectItem value="4">4 Stars</SelectItem>
                    <SelectItem value="5">5 Stars</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="comment">Comment</Label>
                <Textarea
                  id="comment"
                  name="comment"
                  value={currentReview?.comment}
                  onChange={handleInputChange}
                  rows={3}
                />
              </div>
            </div>
            <DialogFooter>
              <Button type="submit">Save</Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>

      <Dialog open={isDeleteDialogOpen} onOpenChange={setIsDeleteDialogOpen}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Confirm Deletion</DialogTitle>
            <DialogDescription>
              Are you sure you want to delete this review? This action cannot be undone.
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button variant="outline" onClick={() => setIsDeleteDialogOpen(false)}>
              Cancel
            </Button>
            <Button variant="destructive" onClick={confirmDelete}>
              Delete
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
