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
package org.jboss.tools.common.model.ui.templates;

import org.eclipse.jface.contentassist.SubjectControlContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.widgets.Text;

/**
 * @author au
 */
public class ControlContentAssistHelper {
	
	public static void createTextContentAssistant(final Text text, IContentAssistProcessor processor, ILabelProvider cueLabel) {
		ContentAssistHandler2.createHandlerForText(text, createJavaContentAssistant(processor), cueLabel);
	}

	public static SubjectControlContentAssistant createJavaContentAssistant(IContentAssistProcessor processor) {
		return org.eclipse.jdt.internal.ui.refactoring.contentassist.ControlContentAssistHelper.createJavaContentAssistant(processor);
	}
	
}
