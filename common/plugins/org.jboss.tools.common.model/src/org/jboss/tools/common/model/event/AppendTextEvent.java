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

public class AppendTextEvent extends XActionAgentEvent {
    public static final int JAVA = 0;
    public static final int XSL = 1;
    public static final int DDL = 2;
    private String title = null;
    private String text;

    public AppendTextEvent(String text) {
        this(DEFAULT, null, text);
    }

    public AppendTextEvent(String id, String title, String text) {
        super();
        this.text = text;
        this.title = title;
        setId(id);
    }

    public String getTitle() {
        return title;
    }

    public String getText() {
        return text;
    }

    public int kind() {
        return JAVA;
    }

}
