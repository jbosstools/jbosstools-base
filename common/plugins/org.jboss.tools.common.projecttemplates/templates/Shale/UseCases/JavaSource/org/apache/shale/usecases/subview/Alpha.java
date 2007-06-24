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

import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>First included page for subview processing use case.</p>
 *
 * $Id: Alpha.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Alpha extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Alpha.class);


    // -------------------------------------------------- ViewController Methods


    public void destroy() {

        record("destroy(alpha)");

    }


    public void init() {

        record("init(alpha)");

    }


    public void preprocess() {

        record("preprocess(alpha)");

    }


    public void prerender() {

        record("prerender(alpha)");

    }


    // --------------------------------------------------------- Private Methods


    private void record(String text) {

        if (log.isDebugEnabled()) {
            log.debug(text);
        }
        Map map = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();
        String actual = (String) map.get("actual");
        if (actual == null) {
            actual = "";
        }
        map.put("actual", actual + text + "/");
        
    }


}
