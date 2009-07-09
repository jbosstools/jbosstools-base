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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.kb.AttributeDescriptor;
import org.jboss.tools.common.kb.KbConnectorFactory;
import org.jboss.tools.common.kb.KbConnectorType;
import org.jboss.tools.common.kb.KbException;
import org.jboss.tools.common.kb.KbTldResource;
import org.jboss.tools.common.kb.TagDescriptor;
import org.jboss.tools.common.kb.wtp.JspWtpKbConnector;
import org.jboss.tools.common.kb.wtp.TLDVersionHelper;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.jst.web.tld.ITaglibMapping;
import org.jboss.tools.jst.web.tld.IWebProject;
import org.jboss.tools.jst.web.tld.WebProjectFactory;

/**
 * 
 * @author eskimo
 *
 */
public class DropUtils {

	public static final String HTML40_URI = ""; //$NON-NLS-1$

	/**
	 * Get TLD's content.
	 * @param input
	 * @param uri
	 * @return
	 */
	public static String getTldContent(IEditorInput input, String uri) {
		String tldContent = null;
		XModel xModel = null;
		if(input instanceof IModelObjectEditorInput) {
			xModel = ((IModelObjectEditorInput)input).getXModelObject().getModel();
		} else if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			XModelObject o = EclipseResourceUtil.getObjectByResource(f);
			if(o != null) xModel = o.getModel();
		}
		if(xModel != null) {
			ITaglibMapping mapping = WebProjectFactory.instance.getWebProject(xModel).getTaglibMapping();
			XModelObject xmo = mapping.getTaglibObject(uri);
			if(xmo != null) {
//				tldLocation = EclipseResourceUtil.getResource(xmo).getFullPath().toString();
				FileAnyImpl fai = (FileAnyImpl)xmo;
				tldContent = fai.getAsText();
			}
		}
		return tldContent;
	}

	/**
	 * Get jsp tag's attribute data from prompting knowledgebase, these data needs for editing in property list
	 * @param document - document where tag will be inserted 
	 * @param uri - uri for tag
	 * @param tagPrefix - prefix for tag
	 * @param tagName - name for tag
	 * @return Array of TagAttributesComposite.AttributeDescriptorValue
	 */
	public static final TagAttributesComposite.AttributeDescriptorValue[] getJspTagAtttributeValueArray(
		IEditorInput input,
		IDocument document,
		String uri,
		String libraryVersion,
		String tagPrefix,
		String tagName
	) {
		JspWtpKbConnector wtpKbConnector;
		List attributes = null;
		String tldContent = getTldContent(input, uri);
		String tldLocation = ""; //$NON-NLS-1$
		try {
			wtpKbConnector = (JspWtpKbConnector)KbConnectorFactory.getIntstance().createConnector(KbConnectorType.JSP_WTP_KB_CONNECTOR, document);
			String version = libraryVersion;
			if(libraryVersion==null || libraryVersion.trim().length()<1) {
				version = TLDVersionHelper.getTldVersion(uri, tagPrefix, document);
			}
			KbTldResource resource = new KbTldResource(uri, tldLocation, tagPrefix, version);
			resource.setTldContent(tldContent);
			wtpKbConnector.registerResource(resource, true);

			TagDescriptor tagInfo = wtpKbConnector.getTagInformation("/"+(tagPrefix==TagProposal.EMPTY_PREFIX?"":tagPrefix+":")+tagName); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			if(tagInfo != null) {
				attributes = tagInfo.getAttributesDescriptors();
			}
		} catch (KbException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		if(attributes==null)return new AttributeDescriptorValue[0]; 
		List<AttributeDescriptorValue> attributesValues = new ArrayList<AttributeDescriptorValue>();
		for (int i = 0; i < attributes.size(); i++) {
			AttributeDescriptor descriptor = (AttributeDescriptor)attributes.get(i);
			attributesValues.add(new AttributeDescriptorValue(descriptor));
		}
		return attributesValues.toArray(new AttributeDescriptorValue[attributesValues.size()]);
	}

	public static final TagDescriptor getJspTagDescriptor(
		IDocument document,
		String uri,
		String libraryVersion,
		String tagPrefix,
		String tagName
	) {
		JspWtpKbConnector wtpKbConnector;
		TagDescriptor tagInfo=null;
		try {
			wtpKbConnector = (JspWtpKbConnector)KbConnectorFactory.getIntstance().createConnector(KbConnectorType.JSP_WTP_KB_CONNECTOR,document);
			String version = libraryVersion;
			if(libraryVersion==null || libraryVersion.trim().length()<1) {
				version = TLDVersionHelper.getTldVersion(uri, tagPrefix, document);
			}
			wtpKbConnector.registerResource(new KbTldResource(uri, "", tagPrefix, version), true);							 //$NON-NLS-1$
			tagInfo = wtpKbConnector.getTagInformation("/"+(tagPrefix==TagProposal.EMPTY_PREFIX?"":tagPrefix+":")+tagName); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		} catch (KbException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		return tagInfo;
	}

	/**
	 * Convert mimeData that contain full file path to IFile instance from project 
	 * @param dropData
	 * @return
	 */
	public static IFile getResourceForMimeData(DropData dropData) {
        // estherbin why in code below create url instance -?
        // URL newUrl = null;
        // URI newUri = null;
        // try {
        // newUri = new URI(dropData.getMimeData());
        // // newUrl = new URL(dropData.getMimeData());
        // } catch (URISyntaxException e) {
        // ModelUIPlugin.getPluginLog().logError(e);
        // }
		boolean isWorkspaceFile = dropData.getMimeData() != null && dropData.getMimeData().startsWith("L/");
        IFile file = isWorkspaceFile ? ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(dropData.getMimeData()).removeFirstSegments(1))
        			: EclipseResourceUtil.getFile(dropData.getMimeData());
        
        if (file == null || !file.exists()) {
            URL newUrl = null;
            try {
                newUrl = new URL(dropData.getMimeData());
                file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(newUrl.getPath()));
                // newUrl = new URL(dropData.getMimeData());
            } catch (MalformedURLException e) {
                ModelUIPlugin.getPluginLog().logError(e);
            }
            file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(newUrl.getPath()));
        }

        return file;
    }	

	/**
	 * Find WEB-ROOT container for given project
	 * @param project
	 * @return
	 */
	public static IContainer getWebRootContainer(IProject project) {
		IContainer container = project; 
		// TODO Eskimo - look how it can be done through WTP EMF model for flexible project
		IModelNature modelNature = EclipseResourceUtil.getModelNature(project);
		if(modelNature == null) return project;
		XModelObject o = modelNature.getModel().getByPath("FileSystems/WEB-ROOT"); //$NON-NLS-1$
		if(o != null) {
			container = (IContainer)EclipseResourceUtil.getResource(o);
		}
		if(container == null) {
			IWebProject p = WebProjectFactory.instance.getWebProject(modelNature.getModel());
			container = ResourcesPlugin.getWorkspace().getRoot().getContainerForLocation(new Path(p.getWebRootLocation()));
		}
		return container;
	}
	
	/**
	 * If it is possible, then converts given {@code path} into URL,
	 * otherwise returns {@code path} unchanged
	 * @return
	 */
	public static String convertPathToUrl(String path) {
		try {
			return new File(path).toURL().toString();
		} catch (MalformedURLException e) {
			return path;
		}
	}
}