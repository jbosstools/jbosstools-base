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

public class RootImpl extends OrderedObjectImpl {
	private static final long serialVersionUID = 1L;
	private Object modifyListener = null;

    public RootImpl() {}

    public boolean isActive() {
        return true;
    }

    public String getLongPath() {
        return "";
    }

    public String getPath() {
        return "";
    }

    public String getPresentationString() {
        String s = getAttributeValue("project name");
        s = (s == null || s.length() == 0) ? super.getPresentationString() : s;
        if(s == null) s = "Workspace";
        return s;
    }

    public void addModifyListener(Object listener) {
        modifyListener = listener;
    }

	public void removeModifyListener(Object listener) {
		if(modifyListener == listener) modifyListener = null;
	}

    public void setModified(boolean value) {
        if(isModified() == value) return;
        super.setModified(value);
        if(modifyListener != null) {
            synchronized (modifyListener) {
                modifyListener.notify();
            }
        }
    }
}
