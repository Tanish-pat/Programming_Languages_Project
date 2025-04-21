"use client"

import type React from "react"

import { useEffect, useState } from "react"
import { useRouter } from "next/navigation"
import { ArrowLeft, Package } from "lucide-react"

import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { Textarea } from "@/components/ui/textarea"
import { Skeleton } from "@/components/ui/skeleton"
import { useToast } from "@/hooks/use-toast"
import { fetchInventoryItem, updateInventoryItem, fetchCategories, fetchSuppliers } from "@/lib/api"

export default function EditInventoryItemPage({ params }: { params: { id: string } }) {
  const router = useRouter()
  const { toast } = useToast()
  const [loading, setLoading] = useState(true)
  const [submitting, setSubmitting] = useState(false)
  const [categories, setCategories] = useState([])
  const [suppliers, setSuppliers] = useState([])
  const [formData, setFormData] = useState({
    name: "",
    description: "",
    category: "",
    supplier: "",
    quantity: "",
    price: "",
    threshold: "",
  })

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Fetch item details, categories, and suppliers from the API
        const [itemData, categoriesData, suppliersData] = await Promise.all([
          fetchInventoryItem(Number.parseInt(params.id)),
          fetchCategories(),
          fetchSuppliers(),
        ])

        setFormData({
          name: itemData.name,
          description: itemData.description || "",
          category: itemData.categoryId?.toString() || "",
          supplier: itemData.supplierId?.toString() || "",
          quantity: itemData.quantity.toString(),
          price: itemData.price.toString(),
          threshold: itemData.threshold.toString(),
        })

        setCategories(categoriesData)
        setSuppliers(suppliersData)
      } catch (error) {
        console.error("Failed to fetch data:", error)
        toast({
          title: "Error",
          description: "Failed to load item data. Please try again.",
          variant: "destructive",
        })
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [params.id, toast])

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value } = e.target
    setFormData((prev) => ({ ...prev, [name]: value }))
  }

  const handleSelectChange = (name: string, value: string) => {
    setFormData((prev) => ({ ...prev, [name]: value }))
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setSubmitting(true)

    try {
      // Convert numeric fields
      const itemData = {
        ...formData,
        quantity: Number.parseInt(formData.quantity),
        price: Number.parseFloat(formData.price),
        threshold: Number.parseInt(formData.threshold),
      }

      // Update inventory item via the API
      await updateInventoryItem(Number.parseInt(params.id), itemData)

      toast({
        title: "Success",
        description: "Inventory item updated successfully",
      })

      router.push(`/inventory/${params.id}`)
    } catch (error) {
      console.error("Failed to update inventory item:", error)
      toast({
        title: "Error",
        description: "Failed to update inventory item. Please try again.",
        variant: "destructive",
      })
    } finally {
      setSubmitting(false)
    }
  }

  if (loading) {
    return (
      <div className="flex min-h-screen w-full flex-col">
        <main className="flex flex-1 flex-col gap-4 p-4 md:gap-8 md:p-8">
          <div className="mx-auto w-full max-w-2xl">
            <div className="flex items-center gap-2 mb-4">
              <Skeleton className="h-10 w-10 rounded-md" />
              <Skeleton className="h-8 w-48" />
            </div>
            <Card>
              <CardHeader>
                <Skeleton className="h-6 w-64" />
                <Skeleton className="h-4 w-48" />
              </CardHeader>
              <CardContent className="space-y-6">
                {Array(6)
                  .fill(0)
                  .map((_, i) => (
                    <div key={i} className="space-y-2">
                      <Skeleton className="h-4 w-24" />
                      <Skeleton className="h-10 w-full" />
                    </div>
                  ))}
                <div className="flex justify-end gap-2">
                  <Skeleton className="h-10 w-24" />
                  <Skeleton className="h-10 w-24" />
                </div>
              </CardContent>
            </Card>
          </div>
        </main>
      </div>
    )
  }

  return (
    <div className="flex min-h-screen w-full flex-col">
      <main className="flex flex-1 flex-col gap-4 p-4 md:gap-8 md:p-8">
        <div className="mx-auto w-full max-w-2xl">
          <div className="flex items-center gap-2 mb-4">
            <Button variant="outline" size="icon" onClick={() => router.back()}>
              <ArrowLeft className="h-4 w-4" />
              <span className="sr-only">Back</span>
            </Button>
            <h1 className="text-2xl font-bold">Edit Item</h1>
          </div>

          <Card>
            <CardHeader>
              <div className="flex items-center gap-2">
                <Package className="h-6 w-6" />
                <CardTitle>Edit Inventory Item</CardTitle>
              </div>
              <CardDescription>Update the details of this inventory item</CardDescription>
            </CardHeader>
            <CardContent>
              <form onSubmit={handleSubmit} className="space-y-6">
                <div className="grid gap-4 md:grid-cols-2">
                  <div className="space-y-2">
                    <Label htmlFor="name">Item Name</Label>
                    <Input
                      id="name"
                      name="name"
                      placeholder="Enter item name"
                      value={formData.name}
                      onChange={handleChange}
                      required
                    />
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="category">Category</Label>
                    <Select
                      value={formData.category}
                      onValueChange={(value) => handleSelectChange("category", value)}
                      required
                    >
                      <SelectTrigger id="category">
                        <SelectValue placeholder="Select category" />
                      </SelectTrigger>
                      <SelectContent>
                        {categories.length > 0 ? (
                          categories.map((category: any) => (
                            <SelectItem key={category.id} value={category.id.toString()}>
                              {category.name}
                            </SelectItem>
                          ))
                        ) : (
                          <>
                            <SelectItem value="electronics">Electronics</SelectItem>
                            <SelectItem value="clothing">Clothing</SelectItem>
                            <SelectItem value="food">Food</SelectItem>
                            <SelectItem value="furniture">Furniture</SelectItem>
                            <SelectItem value="other">Other</SelectItem>
                          </>
                        )}
                      </SelectContent>
                    </Select>
                  </div>
                </div>

                <div className="space-y-2">
                  <Label htmlFor="description">Description</Label>
                  <Textarea
                    id="description"
                    name="description"
                    placeholder="Enter item description"
                    value={formData.description}
                    onChange={handleChange}
                    rows={3}
                  />
                </div>

                <div className="grid gap-4 md:grid-cols-2">
                  <div className="space-y-2">
                    <Label htmlFor="supplier">Supplier</Label>
                    <Select
                      value={formData.supplier}
                      onValueChange={(value) => handleSelectChange("supplier", value)}
                      required
                    >
                      <SelectTrigger id="supplier">
                        <SelectValue placeholder="Select supplier" />
                      </SelectTrigger>
                      <SelectContent>
                        {suppliers.length > 0 ? (
                          suppliers.map((supplier: any) => (
                            <SelectItem key={supplier.id} value={supplier.id.toString()}>
                              {supplier.name}
                            </SelectItem>
                          ))
                        ) : (
                          <>
                            <SelectItem value="acme">Acme Inc.</SelectItem>
                            <SelectItem value="globex">Globex Corporation</SelectItem>
                            <SelectItem value="initech">Initech</SelectItem>
                            <SelectItem value="umbrella">Umbrella Corp</SelectItem>
                            <SelectItem value="other">Other</SelectItem>
                          </>
                        )}
                      </SelectContent>
                    </Select>
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="quantity">Quantity</Label>
                    <Input
                      id="quantity"
                      name="quantity"
                      type="number"
                      placeholder="Enter quantity"
                      value={formData.quantity}
                      onChange={handleChange}
                      min="0"
                      required
                    />
                  </div>
                </div>

                <div className="grid gap-4 md:grid-cols-2">
                  <div className="space-y-2">
                    <Label htmlFor="price">Price ($)</Label>
                    <Input
                      id="price"
                      name="price"
                      type="number"
                      placeholder="Enter price"
                      value={formData.price}
                      onChange={handleChange}
                      min="0"
                      step="0.01"
                      required
                    />
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="threshold">Low Stock Threshold</Label>
                    <Input
                      id="threshold"
                      name="threshold"
                      type="number"
                      placeholder="Enter threshold"
                      value={formData.threshold}
                      onChange={handleChange}
                      min="1"
                      required
                    />
                  </div>
                </div>

                <div className="flex justify-end gap-2">
                  <Button type="button" variant="outline" onClick={() => router.back()}>
                    Cancel
                  </Button>
                  <Button type="submit" disabled={submitting}>
                    {submitting ? "Saving..." : "Save Changes"}
                  </Button>
                </div>
              </form>
            </CardContent>
          </Card>
        </div>
      </main>
    </div>
  )
}
