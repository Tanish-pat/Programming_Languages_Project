// API utility functions to interact with the backend

// Base URL for API requests
const API_BASE_URL = "http://localhost:3000"

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

function toQueryParams(data: Record<string, any>): string {
  const params = new URLSearchParams()
  Object.entries(data).forEach(([key, value]) => {
    if (value !== undefined && value !== null) {
      params.append(key, value.toString())
    }
  })
  return params.toString()
}


// Customer API functions
export const CustomerAPI = {
  getAll: () => fetchAPI<any[]>("/customer/getAll"),
  getById: (id: number) => fetchAPI<any>(`/customer/getById/${id}`),
  create: (data: any) =>
    fetchAPI<any>(`/customer/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (id: number, data: any) =>
    fetchAPI<any>(`/customer/update/${id}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/product/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (sku: string, data: any) =>
    fetchAPI<any>(`/product/update/${sku}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/review/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (reviewId: number, data: any) =>
    fetchAPI<any>(`/review/update/${reviewId}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/address/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (addressId: number, data: any) =>
    fetchAPI<any>(`/address/update/${addressId}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/inventory/update/${productId}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/payment/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (paymentId: number, data: any) =>
    fetchAPI<any>(`/payment/update/${paymentId}?${toQueryParams(data)}`, {
      method: "PUT",
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
    fetchAPI<any>(`/coupon/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (couponCode: string, data: any) =>
    fetchAPI<any>(`/coupon/update/${couponCode}?${toQueryParams(data)}`, {
      method: "PUT",
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
  getById: (categoryId: number) => fetchAPI<any>(`/category/get/${categoryId}`),
  create: (data: any) =>
    fetchAPI<any>(`/category/create?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
  update: (categoryId: number, data: any) =>
    fetchAPI<any>(`/category/update/${categoryId}?${toQueryParams(data)}`, {
      method: "PUT",
    }),
  
  delete: (categoryId: number) =>
    fetchAPI<void>(`/category/delete/${categoryId}`, {
      method: "DELETE",
    }),
}

// ProductCategory API functions
export const ProductCategoryAPI = {
  getAll: () => fetchAPI<any[]>("/product-category/getAll"),
  getByProductAndCategory: (productId: string, categoryId: number) =>
    fetchAPI<any>(`/product-category/get/${productId}/${categoryId}`),
  create: (data: any) =>
    fetchAPI<any>(`/product-category/assign?${toQueryParams(data)}`, {
      method: "POST",
    }),
  
    delete: (productId: string, categoryId: number) =>
      fetchAPI<void>(`/product-category/unassign/?productId=${productId}&categoryId=${categoryId}`, {
        method: "DELETE",
      }),
  getByProduct: (productId: string) => fetchAPI<any[]>(`/product-category/byProduct/${productId}`),
  getByCategory: (categoryId: number) => fetchAPI<any[]>(`/product-category/byCategory/${categoryId}`),
}
