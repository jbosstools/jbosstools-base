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
package org.jboss.tools.common.model.options;

import org.jboss.tools.common.model.*;

public interface SharableElement extends XModelObject, SharableConstants {
    public SharableElement getSharableParent();
    public SharableElement[] getSharableChildren();
    public boolean setName(String name);
    public String name();
    public void setScope(String scopename);
    public String getScope();
    public boolean exists();
    public boolean scopeExists(String scopename);
    public void setScopeExists(String scopename, boolean b);
    public boolean addSharableChild(SharableElement child);
    public void removeSharableChild(String childname);
    public SharableElement findSharableChild(String name);
    public String getAttributeValue(String name, String scopename);
}

