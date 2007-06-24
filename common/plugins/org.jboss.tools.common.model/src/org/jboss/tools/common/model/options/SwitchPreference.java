/*
 * Created on 25.03.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.options;

import java.util.*;


/**
 * @author Eskimo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class SwitchPreference extends Preference {
	
	protected Preference switchPreference = null;

	/**
	 * @param optionPath
	 * @param attributeName
	 */
	public SwitchPreference(String attributeName) {
		super("", attributeName);
		// TODO Auto-generated constructor stub
	}

	public SwitchPreference(String attributeName, Preference switchPreference) {
		this(attributeName);
		this.switchPreference = switchPreference;
	}
	
	public void setSwitchPreference(Preference pref) {
		switchPreference = pref;
	}

	public String getValue() {
		if(switchPreference==null) throw new NullPointerException("switchPreference cannot be null, call setSwitchPreferenceFirst.");
		Preference preference = (Preference)preferencesMap.get(switchPreference.getValue());
		if(preference==null) throw new IllegalStateException("Preference in't added for switch value '" + switchPreference.getValue() + "'"); 
		return preference.getValue();
	}

	public void setValue(String value) {
		if(switchPreference==null) throw new NullPointerException("switchPreference cannot be null, call setSwitchPreferenceFirst.");
		Preference preference = (Preference)preferencesMap.get(switchPreference.getValue());
		if(preference==null) throw new IllegalStateException("Preference in't added for switch value '" + switchPreference.getValue() + "'"); 
		preference.setValue(value);
	}

	protected Hashtable<Object,Preference> preferencesMap = new Hashtable<Object,Preference>();

	public void addPreferenceMapping(Object switchValue,Preference pref) {
		preferencesMap.put(switchValue,pref);
	}
	
	public List<Preference> getPreferences() {
		List<Preference> preferenceList = new ArrayList<Preference>(); 
		Enumeration keys = preferencesMap.keys();
		while(keys.hasMoreElements()) {
			preferenceList.add(
				preferencesMap.get(
					keys.nextElement()
				)
			);
		}
		return Collections.unmodifiableList(preferenceList);
	}
	
	public static final SwitchPreference createSwitchPreference(String name, Preference switchPreference, SwitchPreferenceMapping[] preferenceMap) {
		SwitchPreference preference = new SwitchPreference(name,switchPreference);
		for(int i=0;i<preferenceMap.length;i++) {
			preference.addPreferenceMapping(preferenceMap[i].getValue(),preferenceMap[i].getPreference());
		}
		return preference;		
	}
	
	public static final SwitchPreferenceMapping createMapping(Object value, Preference preference) {
		return new SwitchPreferenceMapping(value,preference);
	}
		
	public static class SwitchPreferenceMapping {
		Object map[];		
		public SwitchPreferenceMapping(Object value, Preference preference) { 
			map= new Object[]{value,preference};
		}
		
		public Object getValue() {
			return map[0];
		}
		
		public Preference getPreference() {
			return (Preference)map[1];
		}	
	}


}
