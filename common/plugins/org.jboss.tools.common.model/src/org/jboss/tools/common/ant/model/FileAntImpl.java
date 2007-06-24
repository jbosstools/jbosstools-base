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
package org.jboss.tools.common.ant.model;

import java.util.*;
import org.jboss.tools.common.ant.parser.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.filesystems.*;
import org.jboss.tools.common.model.filesystems.impl.*;

public class FileAntImpl extends RecognizedFileImpl {
	private static final long serialVersionUID = 1L;
	private boolean loaded = false;
    private String[] targets = null;

    public FileAntImpl() {}

    public boolean hasChildren() {
        return true;
    }

    public boolean isValid() {
        return loaded && targets != null;
    }

    protected void loadChildren() {
        if(loaded || !isActive()) return;
        loaded = true;
        parse();
        if(targets == null) return;
        for (int i = 0; i < targets.length; i++) {
            try {
                Properties p = new Properties();
                p.setProperty("name", targets[i]);
                addChild_0(getModel().createModelObject("AntTarget", p));
            } catch (Exception e) {
            	//ignore
            }
        }
    }

    private void parse() {
        if(targets != null) return;
        String b = getAttributeValue("body");
        try {
            targets = new AntParser(b).getTargets();
        } catch (Exception e) {
        	//ignore
            targets = null;
        }
    }

    public void invalidate() {
        if(!loaded || getParent() == null) return;
        targets = null;
        parse();
        if(targets == null) return;
        children.clear();
        loaded = false;
        fireStructureChanged(XModelTreeEvent.STRUCTURE_CHANGED, getParent());
    }

    public void changeBody(String body) {
        getModel().changeObjectAttribute(this, "body", body);
    }

    public void set(String name, String value) {
        super.set(name, value);
        if("body".equals(name) && loaded) {
            invalidate();
        }
    }

    public XModelObject[] getChildrenForSave() {
        return new XModelObject[0];
    }

    public void setBodySource(BodySource bodysource) {
        super.setBodySource(bodysource);
        invalidate();
    }

}
