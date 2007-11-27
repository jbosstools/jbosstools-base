/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.meta.action;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.event.*;

public interface XAction extends XActionItem, XActionHandler {
    public String HIDE_NEVER = "never";
    public String HIDE_DISABLED = "disabled";
    public String HIDE_ALWAYS = "always";
    
    // standart actions paths
    
	public static final String CUT = "CopyActions.Cut";  
	public static final String COPY = "CopyActions.Copy";  
	public static final String DELETE = "DeleteActions.Delete";  
	public static final String PASTE = "CopyActions.Paste";  

    public String getWizardClassName();
    public XEntityData[] getEntityData(XModelObject object);
    public XRedirect getRedirect();
    public String getBaseActionName();
    public boolean isSave2ModelRequired();
    public boolean hide(boolean enabled);
    
    public String testHandler();
    public String testEntityData();

}

