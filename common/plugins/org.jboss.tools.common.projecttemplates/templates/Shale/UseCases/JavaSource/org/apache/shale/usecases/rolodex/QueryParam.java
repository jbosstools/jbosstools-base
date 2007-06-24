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

package org.apache.shale.usecases.rolodex;

/**
 * <p>This class simulates the classic struts FormBean.  The 
 * request query parameter will be  populated via the managed 
 * bean property setter injection.</p>
 
 */
public class QueryParam {

    /**
     * <p>Holds the value of the target tab index when 
     * clicking on a tab, <code>"#{managed-bean-name.changeTab}"}</code>.
     * </p>
     */
    private String tabIndex = null;
    
    /**
     * <p>Holds the selected name when clicking on a contact,
     * <code>"#{managed-bean-name.selectContact}"</code>.</p>
     */    
    private String selectedName = null;
    

    /**
     * @return Returns the tabIndex.
     */
    public String getTabIndex() {
        return tabIndex;
    }
    

    /**
     * @param tabIndex The tabIndex to set.
     */
    public void setTabIndex(String tabIndex) {
        this.tabIndex = tabIndex;
    }


    /**
     * @return Returns the selectedName.
     */
    public String getSelectedName() {
        return selectedName;
    }
    

    /**
     * @param selectedName The selectedName to set.
     */
    public void setSelectedName(String selectedName) {
        this.selectedName = selectedName;
    }
        
}
