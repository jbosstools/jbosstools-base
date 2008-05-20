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

public class XActionAgentEvent {
    public static final String BASE = "Base";
    public static final String DEFAULT = BASE + ".Out";
    public static final String COMPILATION = BASE + ".Compilation";
    public static final String SOURCE_VIEW = "Source View";

    private String id = DEFAULT;

    public XActionAgentEvent() {}

    public String id() {
        return id;
    }

    protected void setId(String id) {
        this.id = id;
    }

}
