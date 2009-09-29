package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.propertiesfileeditor.IPropertiesFilePartitions;
import org.eclipse.jdt.internal.ui.propertiesfileeditor.PropertiesFileSourceViewerConfiguration;
import org.eclipse.jdt.ui.text.IColorManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;

public class PropertiesSourceViewerConfiguration extends PropertiesFileSourceViewerConfiguration {

	ContentAssistant fContentAssistant;

	public PropertiesSourceViewerConfiguration(IColorManager colorManager,
			IPreferenceStore preferenceStore, ITextEditor editor,
			String partitioning) {
		super(colorManager, preferenceStore, editor, partitioning);

	}

	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		if(fContentAssistant == null) {
			fContentAssistant = new ContentAssistant();
			IEditorInput input = getEditor().getEditorInput();
			if(input instanceof IFileEditorInput && input instanceof IModelObjectEditorInput) {
				IFile f = ((IFileEditorInput)input).getFile();
				String name = f.getName();
				AbstractPropertiesContentAssistProcessor p = createProcessorByFileName(name);
				if(p != null) {
					XModelObject o = ((IModelObjectEditorInput)input).getXModelObject();
					p.setModelObject(o);
					fContentAssistant.setContentAssistProcessor(p, "__dftl_partition_content_type");
				}
				fContentAssistant.setInformationControlCreator(new IInformationControlCreator() {
					public IInformationControl createInformationControl(Shell parent) {
						return new DefaultInformationControl(parent, JavaPlugin.getAdditionalInfoAffordanceString());
					}
				});
			}
		}
		return fContentAssistant;
	}

	static String EXTENSION_POINT = "org.jboss.tools.common.model.ui.propertiesFileContentAssist";
	
	private AbstractPropertiesContentAssistProcessor createProcessorByFileName(String fileName) {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		if(point == null) return null;
		IConfigurationElement[] cs = point.getConfigurationElements();
		for (IConfigurationElement c: cs) {
			if(fileName.equals(c.getAttribute("fileName"))) {
				try {
					AbstractPropertiesContentAssistProcessor p = (AbstractPropertiesContentAssistProcessor)c.createExecutableExtension("processor");
					return p;
				} catch (CoreException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				} catch (ClassCastException e2) {
					ModelUIPlugin.getPluginLog().logError(e2);
				}
			}			
		}

		return null;
	}
}
