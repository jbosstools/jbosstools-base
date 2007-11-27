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

import org.jboss.tools.common.model.XModelObject;

public interface XActionHandler {
    public XEntityData[] getEntityData(XModelObject object);
    public void setDefaultData(XModelObject object);
    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception;
    public boolean getSignificantFlag(XModelObject object);
    public boolean isEnabled(XModelObject object);
    public boolean hide(boolean enabled);

    public boolean isEnabled(XModelObject object, XModelObject[] objects);
    public void executeHandler(XModelObject object, XModelObject[] objects, java.util.Properties p) throws Exception;

}
 
