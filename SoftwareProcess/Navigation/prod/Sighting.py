from Navigation.prod.Angle import Angle
class Sighting(object):

    def __init__(self):
        self.body = ""
        self.date = ""
        self.time = ""
        self.observation = Angle()
        self.height = ""
        self.temperature = ""
        self.pressure = ""
        self.horizon = ""
        self.index = ""
        self.adjustedAltitude = Angle()


#    Getter
    def get_body(self):
        return self.body


    def get_date(self):
        return self.date


    def get_time(self):
        return self.time


    def get_observationString(self):
        return self.observation.getString()
    
    
    def get_observation(self):
        return self.observation.getDegrees()

    def get_height(self):
        return self.height


    def get_temperature(self):
        return self.temperature


    def get_pressure(self):
        return self.pressure


    def get_horizon(self):
        return self.horizon

    
    def get_index(self):
        return self.index


    def get_adjustedAltitude(self):
        return self.adjustedAltitude.getString()
    

#    Setter
    def set_body(self, value):
        self.body = value


    def set_date(self, value):
        self.date = value


    def set_time(self, value):
        self.time = value


    def set_observation(self, value):
        self.observation.setDegreesAndMinutes(value)


    def set_height(self, value = "0"):
        self.height = value


    def set_temperature(self, value = "72"):
        self.temperature = value


    def set_pressure(self, value = "1010"):
        self.pressure = value


    def set_horizon(self, value = "Natural"):
        self.horizon = value
       
        
    def set_index(self):
        self.index = self.date + self.time
    
    
    def set_adjustedAltitude(self, value):
        self.adjustedAltitude.setDegreesAndMinutes(value)