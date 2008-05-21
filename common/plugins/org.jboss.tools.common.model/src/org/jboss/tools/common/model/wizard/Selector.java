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
package org.jboss.tools.common.model.wizard;

import org.jboss.tools.common.meta.action.*;

//TODO check id this class is still needed
public class Selector {
    protected String title = "Select";
    protected String message = null;
    protected String value = "";
    protected String[] values = new String[0];

    public Selector() {}

    public String getSelectedValue() {
        return value;
    }

    public String[] getValues() {
        return values;
    }

    public String getTitle() {
        return title;
    }

    public String getMessage() {
        return message;
    }

    public void setSelectedValue(String v) {
        value = v;
    }

    public void setValues(String[] v) {
        values = v;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public int invoke() {
        SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("XXX");
        if(wizard == null) return -2;
        wizard.setObject(this);
        return wizard.execute();
    }

}
