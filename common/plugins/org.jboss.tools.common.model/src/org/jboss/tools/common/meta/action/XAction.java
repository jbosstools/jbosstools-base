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
    public String HIDE_NEVER = "never"; //$NON-NLS-1$
    public String HIDE_DISABLED = "disabled"; //$NON-NLS-1$
    public String HIDE_ALWAYS = "always"; //$NON-NLS-1$
    
    // standart actions paths
    
	public static final String CUT = "CopyActions.Cut";   //$NON-NLS-1$
	public static final String COPY = "CopyActions.Copy";   //$NON-NLS-1$
	public static final String DELETE = "DeleteActions.Delete";   //$NON-NLS-1$
	public static final String PASTE = "CopyActions.Paste";   //$NON-NLS-1$

    public String getWizardClassName();
    public XEntityData[] getEntityData(XModelObject object);
    public XRedirect getRedirect();
    public String getBaseActionName();
    public boolean isSave2ModelRequired();
    public boolean hide(boolean enabled);
    
    public String testHandler();
    public String testEntityData();

}

