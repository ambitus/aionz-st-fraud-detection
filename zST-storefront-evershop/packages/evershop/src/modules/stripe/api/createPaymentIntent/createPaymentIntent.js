const { select } = require('@evershop/postgres-query-builder');
const smallestUnit = require('zero-decimal-currencies');
const stripePayment = require('stripe');
const { pool } = require('@evershop/evershop/src/lib/postgres/connection');
const { getSetting } = require('../../../setting/services/setting');
const { getConfig } = require('@evershop/evershop/src/lib/util/getConfig');
const {
  OK,
  INVALID_PAYLOAD
} = require('@evershop/evershop/src/lib/util/httpStatus');

// eslint-disable-next-line no-unused-vars
module.exports = async (request, response, delegate, next) => {
  // eslint-disable-next-line camelcase
  const { order_id } = request.body;
  // Check the order
  const order = await select()
    .from('order')
    .where('uuid', '=', order_id)
    .load(pool);

  if (!order) {
    response.status(INVALID_PAYLOAD);
    response.json({
      error: {
        status: INVALID_PAYLOAD,
        message: 'Invalid order'
      }
    });
  } else {
    // **Remove for fraud demo**
    // const stripeConfig = getConfig('system.stripe', {});
    // let stripeSecretKey;
    // if (stripeConfig.secretKey) {
    //   stripeSecretKey = stripeConfig.secretKey;
    // } else {
    //   stripeSecretKey = await getSetting('stripeSecretKey', '');
    // }

    // const stripe = stripePayment(stripeSecretKey);
    // // Create a PaymentIntent with the order amount and currency
    // const paymentIntent = await stripe.paymentIntents.create({
    //   amount: smallestUnit.default(order.grand_total, order.currency),
    //   currency: order.currency,
    //   metadata: {
    //     // eslint-disable-next-line camelcase
    //     orderId: order_id
    //   }
    // });

    response.status(OK);
    response.json({
      data: {
        clientSecret: "pi_1GszrV2eZvKYlo2CsTWUBkrP_secret_srR3g0KVFbMj1y2wEMB55BjcV" // **Remove for fraud demo**paymentIntent.client_secret 
      }
    });
  }
};