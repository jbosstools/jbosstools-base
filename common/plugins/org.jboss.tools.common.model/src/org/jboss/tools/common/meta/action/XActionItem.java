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

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.XMetaElement;

public interface XActionItem extends XMetaElement {
	public String getIconKey();
    public Image getImage();
    public XActionItem getItem(String name);
    public String getPath();
    public String getProperty(String name);

    public interface Acceptor {
        public boolean accepts(XActionItem item);
    }
    public XActionItem copy(Acceptor acceptor);
}
 
