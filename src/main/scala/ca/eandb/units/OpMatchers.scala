package ca.eandb.units

object OpMatchers {

  object * {
    def unapply(units: Units): Option[(Units, Units)] = units match {
      case BinProdUnits(a, b) => Some(a -> b)
      case ProductUnits(a :: b :: Nil) => Some(a -> b)
      case ProductUnits(a :: bs) => Some(a -> ProductUnits(bs))
      case _ => None
    }
  }

  object + {
    def unapply(units: Units): Option[(Units, Units)] = units match {
      case SumUnits(a :: b :: Nil) => Some(a -> b)
      case SumUnits(a :: bs) => Some(a -> SumUnits(bs))
      case _ => None
    }
  }

  object / {
    def unapply(units: Units): Option[(Units, Units)] = units match {
      case BinProdUnits(a, ReciprocalUnits(b)) => Some(a -> b)
      case ProductUnits(a :: ReciprocalUnits(b) :: Nil) => Some(a -> b)
      case ProductUnits(terms) => terms.last match {
        case ReciprocalUnits(b) => Some(ProductUnits(terms.init) -> b)
        case _ => None
      }
      case _ => None
    }
  }

  object ~ {
    def unapply(units: Units): Option[(Units, Int)] = units match {
      case PowerUnits(a, n) => Some(a -> n)
      case _ => None
    }
  }

}
