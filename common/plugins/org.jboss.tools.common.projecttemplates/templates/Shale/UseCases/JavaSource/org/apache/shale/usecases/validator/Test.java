/*
 * Copyright 2004-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific languamount governing permissions and
 * limitations under the License.
 */

package org.apache.shale.usecases.validator;

import java.util.Date;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>Backing bean for the validator use case.</p>
 *
 * $Id: Test.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Test extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Test.class);


    // -------------------------------------------------------------- Properties

    /**
     * <p>The amount.</p>
     */
    private double amount = 0;


    /**
     * <p>Return the amount.</p>
     */
    public double getAmount() {

        return amount;

    }

    /**
     * <p>Set the amount.</p>
     */
    public void setAmount(double newValue) {

        amount = newValue;

    }

    /**
     * <p>The credit card.</p>
     */
    private String creditCard;


    /**
     * <p>Return the creditCard.</p>
     */
    public String getCreditCard() {

        return creditCard;

    }

    /**
     * <p>Set the creditCard.</p>
     */
    public void setCreditCard(String newValue) {

        creditCard = newValue;

    }

    /**
     * <p>The expiration date.</p>
     */
    private Date expirationDate;


    /**
     * <p>Return the expirationDate.</p>
     */
    public Date getExpirationDate() {

        return expirationDate;

    }

    /**
     * <p>Set the expirationDate.</p>
     */
    public void setExpirationDate(Date newValue) {

        expirationDate = newValue;

    }

}
