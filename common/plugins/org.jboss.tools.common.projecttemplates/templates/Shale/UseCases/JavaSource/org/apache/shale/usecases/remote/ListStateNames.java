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

package org.apache.shale.usecases.remote;

import org.apache.shale.remote.AbstractListCompletions;
import org.apache.shale.remote.RemoteContext;
import org.apache.shale.usecases.view.Domains;

/**
 * <p>Remote command to list the state names that match the prefix specified
 * by the <code>prefix</code> request parameter.  If there are no such matching
 * names, zero elements are returned.  If no prefix is specified, all possible
 * names are returned.</p>
 *
 * $Id: ListStateNames.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class ListStateNames extends AbstractListCompletions {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The context attribute key under which our {@link Domains} object
     * may be found.</p>
     */
    private static final String DOMAINS = "domains";


    // ------------------------------------------------------- Protected Methods


    /**
     * <p>Return the set of legal state names.</p>
     *
     * @param context {@link RemoteContext} for this request
     * @param test Test value optionally used to select legal values
     */
    protected String[] legal(RemoteContext context, String test) {

        Domains domains = (Domains) context.getContextAttributes().get(DOMAINS);
        if (domains == null) {
            // FIXME - fake managed beans setup because we are not a JSF request
            domains = new Domains();
            context.getContextAttributes().put(DOMAINS, domains);
        }
        return domains.getStateNames();

    }


}
