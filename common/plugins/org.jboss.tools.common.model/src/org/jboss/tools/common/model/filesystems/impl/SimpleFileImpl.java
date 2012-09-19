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
package org.jboss.tools.common.model.filesystems.impl;

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.filesystems.impl.AbstractExtendedXMLFileImpl;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.loaders.impl.SerializingLoader;
import org.jboss.tools.common.model.util.*;

public class SimpleFileImpl extends AbstractExtendedXMLFileImpl {
    private static final long serialVersionUID = 7216374720598645585L;

    public SimpleFileImpl() {}

    protected RegularChildren createChildren() {
        return new OrderedChildren();
    }

	SerializingLoader loader = null;

    public String get(String name) {
        String v = super.get(name);
        return (!XModelObjectConstants.ATTR_NAME_BODY.equals(name)) ? v : getBody();
    }
    
    public String getBody() {
		if(isIncorrect()) return get(ATTR_NAME_INCORRECT_BODY);
		String abts = get("actualBodyTimeStamp"); //$NON-NLS-1$
		if(abts != null && (abts.equals("0") || abts.equals("" + getTimeStamp()))) { //$NON-NLS-1$ //$NON-NLS-2$
			return get(ATTR_NAME_CORRECT_BODY);
		}
		if(loader == null) loader = (SerializingLoader)XModelObjectLoaderUtil.getObjectLoader(this);
		String body = loader.serializeObject(this);
		set(ATTR_NAME_CORRECT_BODY, body);
		set("actualBodyTimeStamp", "" + getTimeStamp()); //$NON-NLS-1$ //$NON-NLS-2$
		return body;
    }

}

