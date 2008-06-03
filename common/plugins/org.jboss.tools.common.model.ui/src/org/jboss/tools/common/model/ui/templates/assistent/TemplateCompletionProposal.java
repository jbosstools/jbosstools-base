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
package org.jboss.tools.common.model.ui.templates.assistent;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * @author au
 */
public class TemplateCompletionProposal implements ICompletionProposal {

    private String displayName;
    
    public TemplateCompletionProposal(String displayName) {
        this.displayName = displayName;
    }
    
    public void apply(IDocument document) {
    }

    public Point getSelection(IDocument document) {
        return null;
    }

    public String getAdditionalProposalInfo() {
        return null;
    }

    public String getDisplayString() {
        return displayName;
    }

    public Image getImage() {
        return null;
    }

    public IContextInformation getContextInformation() {
        return null;
    }

}
