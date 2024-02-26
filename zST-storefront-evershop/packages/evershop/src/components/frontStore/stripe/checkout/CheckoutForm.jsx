import PropTypes from 'prop-types';
import React, { useState, useEffect } from 'react';
import { CardElement, useStripe, useElements } from '@stripe/react-stripe-js';
import { useQuery } from 'urql';
import { useCheckout } from '@components/common/context/checkout';
import './CheckoutForm.scss';
import { Field } from '@components/common/form/Field';
import TestCards from './TestCards';

const cartQuery = `
  query Query($cartId: String) {
    cart(id: $cartId) {
      billingAddress {
        cartAddressId
        fullName
        postcode
        telephone
        country {
          name
          code
        }
        province {
          name
          code
        }
        city
        address1
        address2
      }
      shippingAddress {
        cartAddressId
        fullName
        postcode
        telephone
        country {
          name
          code
        }
        province {
          name
          code
        }
        city
        address1
        address2
      }
      customerEmail
    }
  }
`;

const cardStyle = {
  style: {
    base: {
      color: '#737373',
      fontFamily: 'Arial, sans-serif',
      fontSmoothing: 'antialiased',
      fontSize: '16px',
      '::placeholder': {
        color: '#737373'
      }
    },
    invalid: {
      color: '#fa755a',
      iconColor: '#fa755a'
    }
  },
  hidePostalCode: true
};

// **Remove for fraud demo**
// export default function CheckoutForm({ stripePublishableKey }) {
export default function CheckoutForm() {
  const app_url_w_port = 'ip:port'
  const [, setSucceeded] = useState(false);
  const [cardComleted, setCardCompleted] = useState(false);
  const [error, setError] = useState(null);
  const [, setDisabled] = useState(true);
  const [clientSecret, setClientSecret] = useState('');
  const [showTestCard, setShowTestCard] = useState('success');
  const stripe = useStripe();
  const elements = useElements();
  const { cartId, orderId, orderPlaced, paymentMethods, checkoutSuccessUrl } =
    useCheckout();

  const [result] = useQuery({
    query: cartQuery,
    variables: {
      cartId
    },
    pause: orderPlaced === true
  });

  useEffect(() => {
    // Create PaymentIntent as soon as the order is placed
    if (orderId) {
      window
        .fetch('/api/stripe/paymentIntents', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ order_id: orderId })
        })
        .then((res) => res.json())
        .then((data) => {
          setClientSecret(data.data.clientSecret);
        });
    }
  }, [orderId]);

  useEffect(() => {
    const pay = async () => {
      // **Remove for fraud demo**
      // const billingAddress =
      //   result.data.cart.billingAddress || result.data.cart.shippingAddress;
      // const payload = await stripe.confirmCardPayment(clientSecret, {
      //   payment_method: {
      //     card: elements.getElement(CardElement),
      //     billing_details: {
      //       name: billingAddress.fullName,
      //       email: result.data.cart.customerEmail,
      //       phone: billingAddress.telephone,
      //       address: {
      //         line1: billingAddress.address1,
      //         country: billingAddress.country.code,
      //         state: billingAddress.province.code,
      //         postal_code: billingAddress.postcode,
      //         city: billingAddress.city
      //       }
      //     }
      //   }
      // });

      // if (payload.error) {
      //   setError(`Payment failed ${payload.error.message}`);
      // } else {

      var transaction_data = "";
      if (showTestCard == 'success'){
        transaction_data = '{"User": "0", "Month": "9", "Amount": "134.09", "Day": "1", "Merchant Name": "3527213246127876953", "Year": "2002", "Zip": "91750.0", "Card": "0", "Use Chip": "Swipe Transaction", "Time": 621}';
      }
      else{
        transaction_data = '{"User": "1997", "Month": "9", "Amount": "188.26", "Day": "4", "Merchant Name": "3189517333335617109", "Year": "2016", "Zip": "0", "Card": "2", "Use Chip": "Online Transaction", "Time": 1355}';
      }

      const fraud_check = await fetch('http://' + app_url_w_port + '/fraud_detector', {
          method: 'POST',
          headers: {
              'Accept': 'application/json',
              'Content-Type': 'application/json'
          },
          body: transaction_data
      })
         .then(response => response.json())
         .then(response => {

            if (response["prediction"] == "Not fraud"){

              setError(null);
              setSucceeded(true);

              // Redirect to checkout success page
              window.location.href = `${checkoutSuccessUrl}/${orderId}`;
            }
            else{
              setError('Payment failed. Please contact your credit card provider for details. Refresh page to continue.');
            }
        });
    };

    if (orderPlaced === true && clientSecret) {
      pay();
    }
  }, [orderPlaced, clientSecret, result]);

  const handleChange = (event) => {
    // Listen for changes in the CardElement
    // and display any errors as the customer types their card details
    setDisabled(event.empty);
    if (event.complete === true && !event.error) {
      setCardCompleted(true);
    }
  };

  const testSuccess = () => {
    setShowTestCard('success');
  };

  const testFailure = () => {
    setShowTestCard('failure');
  };

  if (result.error) {
    return (
      <p>
        Oh no...
        {error.message}
      </p>
    );
  }
  // Check if the selected payment method is Stripe
  const stripePaymentMethod = paymentMethods.find(
    (method) => method.code === 'stripe' && method.selected === true
  );
  if (!stripePaymentMethod) return null;

  return (
    // eslint-disable-next-line react/jsx-filename-extension
    <div>
      <div className="stripe-form">

        {/* **Remove for fraud demo** stripePublishableKey && stripePublishableKey.startsWith('pk_test') &&*/ (
          <TestCards
            showTestCard={showTestCard}
            testSuccess={testSuccess}
            testFailure={testFailure}
          />
        )}
        <CardElement
          id="card-element"
          options={cardStyle}
          onChange={handleChange}
        />
      </div>
      {/* Show any error that happens when processing the payment */}
      {error && (
        <div className="card-error text-critical mb-2" role="alert">
          {error}
        </div>
      )}
      {/* **Remove for fraud demo** <Field
        type="hidden"
        name="stripeCartComplete"
        value={cardComleted ? 1 : ''}
        validationRules={[
          {
            rule: 'notEmpty',
            message: 'Please complete the card information'
          }
        ]}
      />*/}
    </div>
  );
}

// **Remove for fraud demo**
// CheckoutForm.propTypes = {
//   stripePublishableKey: PropTypes.string.isRequired
// };