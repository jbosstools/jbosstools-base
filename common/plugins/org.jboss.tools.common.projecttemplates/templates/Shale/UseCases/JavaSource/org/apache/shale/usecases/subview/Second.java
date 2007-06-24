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
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shale.usecases.subview;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>Second page for subview processing use case.</p>
 *
 * $Id: Second.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Second extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Second.class);


    // -------------------------------------------------------------- Properties


    /**
     * <p>Return the expected trace string for the seonc page, which depends
     * on whether or not this is a postback.</p>
     */
    public String getExpected() {

        if (isPostBack()) {
            return "init(gamma)/preprocess(gamma)/init(delta)/preprocess(delta)/" +
                   "prerender(gamma)/prerender(delta)/";
        } else {
            return "init(alpha)/preprocess(alpha)/init(beta)/preprocess(beta)/" +
                   "init(gamma)/prerender(gamma)/init(delta)/prerender(delta)/";
        }

    }


    // -------------------------------------------------- ViewController Methods


    public void destroy() {

        if (log.isDebugEnabled()) {
            log.debug("destroy(second)");
        }

    }


    public void init() {

        if (log.isDebugEnabled()) {
            log.debug("init(second, postBack=" + isPostBack() + ")");
        }

    }


    public void preprocess() {

        if (log.isDebugEnabled()) {
            log.debug("preprocess(second)");
        }

    }


    public void prerender() {

        if (log.isDebugEnabled()) {
            log.debug("prerender(second)");
        }

    }


}
