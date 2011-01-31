/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.common.model.ui.wizards;

import org.eclipse.jface.wizard.IWizard;

/**
 * Wizard run by JavaHyperlinkLineFieldEditor as new Java type wizard.
 * 
 * @author  Viachesld.av Kabanovich
 *
 */
public interface INewClassWizard extends IWizard {

	public void setAdapter(NewTypeWizardAdapter adapter);

	public String getQualifiedClassName();

}
