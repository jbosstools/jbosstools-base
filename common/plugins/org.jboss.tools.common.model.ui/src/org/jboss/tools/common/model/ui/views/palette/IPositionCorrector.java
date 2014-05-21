/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.views.palette;

import org.eclipse.jface.text.ITextSelection;
import org.w3c.dom.Document;

/**
 * Implementation of this interface is bound to palette item by extension insertTagWizard.
 * Instance is created by PaletteInsertManager and is used 
 * a) in PaletteInsertHelper.correctSelection() to define the proper position
 *    for inserting the snippet near cursor position
 * b) in palette wizards to show warning if the is no proper position for the snippet.
 *  
 * @author Viacheslav Kabanovich
 *
 */
public interface IPositionCorrector {

	/**
	 * Defines the proper position to insert he snippet bound to this
	 * implementation. Implementation should return unmodified selection 
	 * either if it is good or if there is no proper position 
	 * for the snipped in the document.  
	 * 
	 * @param document
	 * @param selection
	 * @return
	 */
	public ITextSelection correctSelection(Document document, ITextSelection selection);

	/**
	 * Checks if there exists a proper position for inserting the snippet bound to this
	 * implementation. Returns null if such a position exists, otherwise returns warning
	 * message.
	 * 
	 * @param document
	 * @param selection
	 * @return
	 */
	public String getWarningMessage(Document document, ITextSelection selection);
}
