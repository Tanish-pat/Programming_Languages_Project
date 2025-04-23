// API utility functions to interact with the backend

// Base URL for API requests
const API_BASE_URL = "http://localhost:3000/api"

// Generic fetch function with error handling
async function fetchAPI<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
  const url = `${API_BASE_URL}${endpoint}`

  try {
    const response = await fetch(url, {
      ...options,
      headers: {
        "Content-Type": "application/json",
        ...options.headers,
      },
    })

    if (!response.ok) {
      const error = await response.json()
      throw new Error(error.message || "An error occurred")
    }

    return await response.json()
  } catch (error) {
    console.error(`API Error: ${error}`)
    throw error
  }
}

// Customer API functions
export const CustomerAPI = {
  getAll: () => fetchAPI<any[]>("/customer/getAll"),
  getById: (id: number) => fetchAPI<any>(`/customer/getById/${id}`),
  create: (data: any) =>
    fetchAPI<any>("/customer/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (id: number, data: any) =>
    fetchAPI<any>(`/customer/update/${id}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (id: number) =>
    fetchAPI<void>(`/customer/delete/${id}`, {
      method: "DELETE",
    }),
  search: (params: { name?: string; age?: number }) => {
    const searchParams = new URLSearchParams()
    if (params.name) searchParams.append("name", params.name)
    if (params.age) searchParams.append("age", params.age.toString())

    return fetchAPI<any[]>(`/customer/search?${searchParams.toString()}`)
  },
}

// Product API functions
export const ProductAPI = {
  getAll: () => fetchAPI<any[]>("/product/getAll"),
  getBySku: (sku: string) => fetchAPI<any>(`/product/getBySku/${sku}`),
  create: (data: any) =>
    fetchAPI<any>("/product/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (sku: string, data: any) =>
    fetchAPI<any>(`/product/update/${sku}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (sku: string) =>
    fetchAPI<void>(`/product/delete/${sku}`, {
      method: "DELETE",
    }),
  search: (params: { name?: string; tags?: string }) => {
    const searchParams = new URLSearchParams()
    if (params.name) searchParams.append("name", params.name)
    if (params.tags) searchParams.append("tags", params.tags)

    return fetchAPI<any[]>(`/product/search?${searchParams.toString()}`)
  },
}

// Review API functions
export const ReviewAPI = {
  getAll: () => fetchAPI<any[]>("/review/getAll"),
  getById: (reviewId: number) => fetchAPI<any>(`/review/getById/${reviewId}`),
  create: (data: any) =>
    fetchAPI<any>("/review/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (reviewId: number, data: any) =>
    fetchAPI<any>(`/review/update/${reviewId}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (reviewId: number) =>
    fetchAPI<void>(`/review/delete/${reviewId}`, {
      method: "DELETE",
    }),
  getByProduct: (productId: string, minRating?: number) => {
    const searchParams = new URLSearchParams()
    if (minRating) searchParams.append("minRating", minRating.toString())

    return fetchAPI<any[]>(`/review/product/${productId}?${searchParams.toString()}`)
  },
}

// Address API functions
export const AddressAPI = {
  getAll: () => fetchAPI<any[]>("/address/getAll"),
  getById: (addressId: number) => fetchAPI<any>(`/address/getById/${addressId}`),
  create: (data: any) =>
    fetchAPI<any>("/address/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (addressId: number, data: any) =>
    fetchAPI<any>(`/address/update/${addressId}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (addressId: number) =>
    fetchAPI<void>(`/address/delete/${addressId}`, {
      method: "DELETE",
    }),
  getByCustomer: (customerId: number) => fetchAPI<any[]>(`/address/byCustomer/${customerId}`),
}

// Inventory API functions
export const InventoryAPI = {
  getAll: () => fetchAPI<any[]>("/inventory/getAll"),
  getByProduct: (productId: string) => fetchAPI<any>(`/inventory/getByProduct/${productId}`),
  update: (productId: string, data: any) =>
    fetchAPI<any>(`/inventory/update/${productId}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  checkAvailability: (location?: string) => {
    const searchParams = new URLSearchParams()
    if (location) searchParams.append("location", location)

    return fetchAPI<any[]>(`/inventory/availability?${searchParams.toString()}`)
  },
}

// Payment API functions
export const PaymentAPI = {
  getAll: () => fetchAPI<any[]>("/payment/getAll"),
  getById: (paymentId: number) => fetchAPI<any>(`/payment/getById/${paymentId}`),
  create: (data: any) =>
    fetchAPI<any>("/payment/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (paymentId: number, data: any) =>
    fetchAPI<any>(`/payment/update/${paymentId}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (paymentId: number) =>
    fetchAPI<void>(`/payment/delete/${paymentId}`, {
      method: "DELETE",
    }),
}

// Coupon API functions
export const CouponAPI = {
  getAll: () => fetchAPI<any[]>("/coupon/getAll"),
  getByCode: (couponCode: string) => fetchAPI<any>(`/coupon/getByCode/${couponCode}`),
  create: (data: any) =>
    fetchAPI<any>("/coupon/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (couponCode: string, data: any) =>
    fetchAPI<any>(`/coupon/update/${couponCode}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (couponCode: string) =>
    fetchAPI<void>(`/coupon/delete/${couponCode}`, {
      method: "DELETE",
    }),
  getActive: () => fetchAPI<any[]>("/coupon/active"),
}

// Category API functions
export const CategoryAPI = {
  getAll: () => fetchAPI<any[]>("/category/getAll"),
  getById: (categoryId: number) => fetchAPI<any>(`/category/getById/${categoryId}`),
  create: (data: any) =>
    fetchAPI<any>("/category/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  update: (categoryId: number, data: any) =>
    fetchAPI<any>(`/category/update/${categoryId}`, {
      method: "PUT",
      body: JSON.stringify(data),
    }),
  delete: (categoryId: number) =>
    fetchAPI<void>(`/category/delete/${categoryId}`, {
      method: "DELETE",
    }),
}

// ProductCategory API functions
export const ProductCategoryAPI = {
  getAll: () => fetchAPI<any[]>("/productCategory/getAll"),
  getByProductAndCategory: (productId: string, categoryId: number) =>
    fetchAPI<any>(`/productCategory/get/${productId}/${categoryId}`),
  create: (data: any) =>
    fetchAPI<any>("/productCategory/create", {
      method: "POST",
      body: JSON.stringify(data),
    }),
  delete: (productId: string, categoryId: number) =>
    fetchAPI<void>(`/productCategory/delete/${productId}/${categoryId}`, {
      method: "DELETE",
    }),
  getByProduct: (productId: string) => fetchAPI<any[]>(`/productCategory/byProduct/${productId}`),
  getByCategory: (categoryId: number) => fetchAPI<any[]>(`/productCategory/byCategory/${categoryId}`),
}
