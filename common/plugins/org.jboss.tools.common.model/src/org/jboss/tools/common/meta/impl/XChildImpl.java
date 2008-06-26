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
package org.jboss.tools.common.meta.impl;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.XChild;

public class XChildImpl extends XMetaElementImpl implements XChild {
    protected boolean required;
    protected int count;

    public XChildImpl() {}

    public boolean isRequired() {
        return required;
    }

    public int getMaxCount() {
        return count;
    }

    public void load(Element element){
        setName(element.getAttribute(NAME));
        required = XMetaDataLoader.getBoolean(element, REQUIRED, false);
        count = XMetaDataLoader.getInt(element, MAX_COUNT, Integer.MAX_VALUE);
    }

}
