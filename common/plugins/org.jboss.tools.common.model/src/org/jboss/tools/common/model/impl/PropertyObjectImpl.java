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

public class PropertyObjectImpl extends RegularObjectImpl {
	private static final long serialVersionUID = 1L;
	static int MAX_VISIBLE_VALUE_LENGTH = 25;

    public PropertyObjectImpl() {}

    public String getPathPart() {
        String name = name();
        if(name == null || !"no".equals(get("ENABLED"))) return super.getPathPart();
        return "#" + name + '=' + get("VALUE");
    }

    public String getPresentationString() {
        String name = name();
        if(name == null) return "";
        if("no".equals(get("ENABLED"))) name = "#" + name;
        String value = get("VALUE");
        if(value == null) value = "";
        if(value.indexOf("\n") >= 0) value = value.substring(0, value.indexOf("\n")) + "...";
        if(value.length() > MAX_VISIBLE_VALUE_LENGTH)
          value = value.substring(0, MAX_VISIBLE_VALUE_LENGTH) + "...";
        return name + '=' + value;
    }


}
