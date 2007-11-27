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
package org.jboss.tools.common.meta;

import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.icons.XIconList;

public interface XModelMetaData {
	public String[] entities();
    public XModelEntity getEntity(String entityname);
    public XActionList getGlobalActions();
    public XMapping getMapping(String name);
    public XIconList getIconList();
    public XParents getParentInfo();
}

