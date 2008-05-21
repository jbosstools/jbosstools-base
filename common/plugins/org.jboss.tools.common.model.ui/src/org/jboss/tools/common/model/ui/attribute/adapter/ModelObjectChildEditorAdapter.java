/*
 * Created on Feb 18, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.StructuredChange;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * @author aleksey
 *
 */
public class ModelObjectChildEditorAdapter extends DefaultValueAdapter 
	implements IStructuredChangeListener, IListContentProvider {
	public interface INewValueProvider {
		public String getValue();
	}
	 
	protected static final String DEFAULT_DELIMITER = ";";
	
	ILabelProvider labelProvider = new LabelProvider();
	INewValueProvider newValueProvider = null;
	
	public Object getAdapter(Class adapter) 
	{
		Object result = null;
		
		if (adapter == INewValueProvider.class) 
			result = newValueProvider;
		else if (adapter == IValueChangeListener.class) 
			result = this;
		else if (adapter == IStructuredChangeListener.class) 
			result = this;
		else if (adapter == IListContentProvider.class) 
			result = this;
		else if (adapter == ILabelProvider.class)
			result = labelProvider;
		else
			result = super.getAdapter(adapter); 
			
		return result;
	}

//		IStructuredChangeListener
	public void structureChanged(StructuredChangedEvent event) 
	{
		StringBuffer newValue = new StringBuffer();
		StructuredChange change = (StructuredChange)event.getChange();
		Iterator iterator = change.iterator();
		if (iterator != null)
			while (iterator.hasNext()) 
			{
				newValue.append(iterator.next());
				if (iterator.hasNext()) newValue.append(DEFAULT_DELIMITER);
			}
		setValue(newValue.toString());
	}

//		IListContentProvider
	public Object[] getElements(Object inputElement) {
		List<String> tokens = new ArrayList<String>();
		StringTokenizer tokenizer = new StringTokenizer(getStringValue(true), DEFAULT_DELIMITER);
		while (tokenizer.hasMoreTokens()) tokens.add(tokenizer.nextToken());

		return tokens.toArray();
	}
	
	public void dispose() {
		super.dispose();
		if (labelProvider!=null) labelProvider.dispose();
		labelProvider = null;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
	
	public void setNewValueProvider(INewValueProvider provider)
	{
		newValueProvider = provider;		
	}
}
