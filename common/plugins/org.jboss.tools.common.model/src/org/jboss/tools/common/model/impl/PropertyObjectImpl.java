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
package org.jboss.tools.common.model.impl;

import org.jboss.tools.common.model.XModelObjectConstants;

public class PropertyObjectImpl extends RegularObjectImpl {
	private static final long serialVersionUID = 1L;
	static int MAX_VISIBLE_VALUE_LENGTH = 25;

    public PropertyObjectImpl() {}

    public String getPathPart() {
        String name = name();
        if(name == null || !XModelObjectConstants.NO.equals(get("ENABLED"))) return super.getPathPart(); //$NON-NLS-1$
        return "#" + name + '=' + get("VALUE"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public String getPresentationString() {
        String name = name();
        if(name == null) return ""; //$NON-NLS-1$
        if(XModelObjectConstants.NO.equals(get("ENABLED"))) name = "#" + name; //$NON-NLS-1$ //$NON-NLS-2$
        String value = get("VALUE"); //$NON-NLS-1$
        if(value == null) value = ""; //$NON-NLS-1$
        if(value.indexOf("\n") >= 0) value = value.substring(0, value.indexOf("\n")) + "..."; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        if(value.length() > MAX_VISIBLE_VALUE_LENGTH)
          value = value.substring(0, MAX_VISIBLE_VALUE_LENGTH) + "..."; //$NON-NLS-1$
        return name + '=' + value;
    }


}
