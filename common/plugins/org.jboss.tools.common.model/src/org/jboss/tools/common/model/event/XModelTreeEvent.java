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
package org.jboss.tools.common.model.event;

import java.util.EventObject;
import org.jboss.tools.common.model.*;

public class XModelTreeEvent extends EventObject {
	private static final long serialVersionUID = 1L;
	public static final int NODE_CHANGED = 0;
    public static final int CHILD_ADDED = 1;
    public static final int CHILD_REMOVED = 2;
    public static final int STRUCTURE_CHANGED = 3;

    private XModelObject object;
    private Object info;
	private Object details;
    private int kind = 0;

    public XModelTreeEvent(XModel source, XModelObject object) {
        super(source);
        this.object = object;
    }

    public XModelTreeEvent(XModel source, XModelObject object, int kind, Object info) {
    	this(source, object, kind, info, null);
    }

	public XModelTreeEvent(XModel source, XModelObject object, int kind, Object info, Object details) {
		super(source);
		this.object = object;
		this.kind = kind;
		this.info = info;
		this.details = details;
	}

    public XModelObject getModelObject() {
		return object;
    }

    public int kind() {
        return kind;
    }

    public Object getInfo() {
        return info;
    }
    
    public Object getDetails() {
    	return details;
    }

}
