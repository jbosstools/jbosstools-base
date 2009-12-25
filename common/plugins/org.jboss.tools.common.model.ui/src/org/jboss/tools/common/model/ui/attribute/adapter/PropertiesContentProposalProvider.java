/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;
import org.jboss.tools.common.model.ui.attribute.AttributeContentProposalProviderFactory;
import org.jboss.tools.common.model.ui.texteditors.propertyeditor.AbstractPropertiesContentAssistProcessor;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public abstract class PropertiesContentProposalProvider implements IContentProposalProvider {
	protected XModelObject object;
	protected XEntityData data;
	protected XAttribute attribute;	

	public PropertiesContentProposalProvider() {}

	/**
	 * Helper method to get java type proposals.
	 * 
	 * @param contents
	 * @param position
	 * @return
	 */
	protected List<IContentProposal> getJavaTypeContentProposals(String contents, int position) {
		List<IContentProposal> result = new ArrayList<IContentProposal>();
		String valuePrefix = contents.substring(0, position);
		JavaClassContentAssistProvider p = new JavaClassContentAssistProvider();
		p.init(object, null, attribute);
		IContentProposalProvider pp = p.getContentProposalProvider();
		IContentProposal[] ps = pp.getProposals(valuePrefix, valuePrefix.length());
		IJavaProject jp = getJavaProject();
		for (int i = 0; i < ps.length; i++) {
			String descr = ps[i].getDescription();
			if (descr == null || descr.length() == 0) {
				String value = ps[i].getContent();
				descr = AbstractPropertiesContentAssistProcessor.getDescription(jp, value);
				IContentProposal p2 = AttributeContentProposalProviderFactory
						.makeContentProposal(value, ps[i].getLabel(), descr);
				result.add(p2);
			} else {
				result.add(ps[i]);
			}
		}
		return result;
	}

	protected String getPropertyName() {
		String value = null;	
		if(data != null) {
			value = data.getValue(XModelObjectConstants.ATTR_NAME);

		} else if(object != null && isPropertyEntity(object.getModelEntity().getName())) {
			value = object.getAttributeValue(XModelObjectConstants.ATTR_NAME);
			
		}
		return value;		
	}

	protected boolean isPropertyEntity(String entity) {
		return PropertiesLoader.ENT_PROPERTY.equals(entity);
	}

	protected boolean isNameAttribute() {
		return attribute.getName().equals(XModelObjectConstants.ATTR_NAME);
	}

	protected IJavaProject getJavaProject() {
		IProject project = EclipseResourceUtil.getProject(object);
		return EclipseResourceUtil.getJavaProject(project);		
	}
}
